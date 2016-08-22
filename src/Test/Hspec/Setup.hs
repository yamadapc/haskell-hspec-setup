{-# LANGUAGE LambdaCase #-}
module Test.Hspec.Setup
  where

import           Prelude                      hiding (readFile)

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Language.Haskell.Exts
import           System.Directory
import           System.Directory.ProjectRoot
import           System.Directory.Recursive
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO                    hiding (readFile)
import           System.IO.Strict             (readFile)
import           System.Process

main :: IO ()
main = do
    mfp <- getData
    case mfp of
        Left e -> panic e
        Right fp -> executeCommand fp
  where
    panic e = do
        hPutStrLn stderr e
        exitFailure
    executeCommand (pr, fp) = do
        as <- getArgs
        case as of
            ("--generate":rest) -> hspecGenerate pr (listToMaybe rest)
            ("-g":rest) -> hspecGenerate pr (listToMaybe rest)
            [] -> hspecSetup pr fp
            _ -> error $ unlines [ "Usage: hspec-setup [-g|--generate]"
                                 , "    hspec-setup                   Adds a test-suite to your project"
                                 , "    hspec-setup --generate [fp]   Generates tests for a certain module"
                                 , "    hspec-setup -g"
                                 ]

data ManifestFilePath = CabalFile FilePath
                      | HpackFile FilePath

unManifestFilePath :: ManifestFilePath -> FilePath
unManifestFilePath (CabalFile fp) = fp
unManifestFilePath (HpackFile fp) = fp

type Options = (FilePath, ManifestFilePath)

getData :: IO (Either String Options)
getData = getProjectRootCurrent >>=
    \case
        Nothing -> return $ Left "Couldn't find the project root"
        Just pr -> do
            fs <- getDirectoryContents pr
            case find ("package.yaml" ==) fs of
                Just fp -> return $ Right (pr, HpackFile fp)
                Nothing -> case find ((".cabal" ==) . takeExtension) fs of
                    Just fp -> return $ Right (pr, CabalFile fp)
                    Nothing -> return $ Left
                        "Couldn't find your package manifest file (package.yaml/*.cabal)"


hspecTestSuite :: String
hspecTestSuite = unlines [ ""
                         , "test-suite hspec"
                         , "  main-is: Spec.hs"
                         , "  type: exitcode-stdio-1.0"
                         , "  build-depends: base"
                         , "               , hspec"
                         , "               , QuickCheck"
                         , "  hs-source-dirs: test"
                         , "  default-language: Haskell2010"
                         ]

hspecHpackTestSuite :: String
hspecHpackTestSuite = unlines [ ""
                              , "tests:"
                              , "  hspec:"
                              , "    main: Spec.hs"
                              , "    source-dirs: test"
                              , "    dependencies:"
                              , "    - base"
                              , "    - hspec"
                              , "    - QuickCheck"
                              ]

hspecDiscoveryFile :: String
hspecDiscoveryFile = "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"

hspecSanitySpec :: String
hspecSanitySpec = unlines [ "module SanitySpec where"
                          , ""
                          , "import Test.Hspec"
                          , ""
                          , "spec = describe \"when I have tests\" $"
                          , "    it \"I have sanity\" $ True `shouldBe` True"
                          ]

hspecGenerate :: FilePath -> Maybe FilePath -> IO ()
hspecGenerate pr (Just target) = parseFile target >>= \case
    ParseOk (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ (Just (ExportSpecList _ exportSpecs)))) _ _ _) ->
        go moduleName (exportsFromSpecs exportSpecs)
    ParseOk (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ Nothing)) _ _ moduleDecls) ->
        go moduleName (exportsFromDecls moduleDecls)
    ParseOk (Module l Nothing _ _ moduleDecls) -> error (target <> " needs a `module` name")
    ParseOk h -> error ("Don't know how to generate code from" <> (show h))
    e -> error (show e)
  where
    exportsFromSpecs = mapMaybe mexportFromSpec
      where
        mexportFromSpec (EVar _ (UnQual _ n)) = Just (prettyPrint n)
        mexportFromSpec _ = Nothing
    exportsFromDecls = mapMaybe mexportFromDecl
      where
        mexportFromDecl (FunBind _ ((Match _ n _ _ _):_)) = Just (prettyPrint n)
        mexportFromDecl _ = Nothing
    go :: String -> [String] -> IO ()
    go moduleName moduleExports = do
        let moduleNameFp = (foldl (</>) "./test" $ splitOn "." (moduleName <> "Spec")) <> ".hs"
            moduleNameFallbackFp = (foldl (</>) "./test" $ splitOn "." (moduleName <> "AutogenSpec")) <> ".hs"
            testSuite mn = init $ unlines $
                        [ "module " <> mn <> " where"
                        , ""
                        , "import           " <> moduleName
                        , ""
                        , "import           Test.Hspec"
                        , ""
                        , "spec :: Spec"
                        , "spec = do"
                        ] <> concatMap decToDescribe moduleExports

        createDirectoryIfMissing True (takeDirectory moduleNameFp)
        e <- doesFileExist moduleNameFp

        when e $ do
            hPutStrLn stderr ("Refusing to overwrite " <> moduleNameFp)
            hPutStrLn stderr "Wrote test-suite to:"
            putStrLn moduleNameFallbackFp
            writeFile moduleNameFallbackFp (testSuite (moduleName <> "AutogenSpec"))
            exitSuccess

        writeFile moduleNameFp (testSuite (moduleName <> "Spec"))
      where
        decToDescribe decName = [
            "    describe \"" <> decName <> "\" $ do"
          , "        it \"works\" pending"
                                ]
hspecGenerate pr Nothing = error "Interactive mode not implemented"

hspecSetup :: FilePath -> ManifestFilePath -> IO ()
hspecSetup pr mfp = do
    let fp = unManifestFilePath mfp

    c <- getCurrentDirectory

    putStrLn $ "Adding test-suite to " <> makeRelative c fp <> "..."

    appendHspecSuite pr mfp

    putStrLn "Creating test directory..."
    createDirectoryIfMissing False (pr </> "test")

    putStrLn "Creating test/Spec.hs discovery file..."
    writeFile (pr </> "test" </> "Spec.hs") hspecDiscoveryFile

    putStrLn "Creating test/SanitySpec.hs..."
    writeFile (pr </> "test" </> "SanitySpec.hs") hspecSanitySpec

    stackInited <- doesFileExist (pr </> "stack.yaml")
    unless stackInited $ do
        putStrLn "No `stack.yaml` found. Running `stack init` for you..."
        callCommand "stack init"

    putStrLn "Running tests for the first time..."
    callCommand "stack test"

    return ()

appendHspecSuite :: FilePath -> ManifestFilePath -> IO ()
appendHspecSuite pr (HpackFile fp) = do
    hpackContents <- readFile (pr </> fp)
    let hpackLines = lines hpackContents
        mi = elemIndex "tests:" hpackLines
    case mi of
        Nothing -> do
            when ("hspec" `isInfixOf` hpackContents) $ do
                hPutStrLn stderr "File already has test-suite. Exiting..."
                exitFailure
            appendFile (pr </> fp) hspecHpackTestSuite
        Just i -> do
            let (beforeTests, afterTests) = splitAt i hpackLines :: ([String], [String])
            writeFile (pr </> fp) $ unlines [ unlines beforeTests
                                            , unlines (drop 1 (lines hspecHpackTestSuite))
                                            , unlines afterTests
                                            ]
appendHspecSuite pr (CabalFile fp) = do
    cabalContents <- readFile (pr </> fp)

    when ("type: exitcode-stdio-1.0" `isInfixOf` cabalContents) $ do
        hPutStrLn stderr "File already has test-suite. Exiting..."
        exitFailure

    appendFile (pr </> fp) hspecTestSuite

