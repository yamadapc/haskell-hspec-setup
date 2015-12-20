module Main
  where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           System.Directory
import           System.Directory.ProjectRoot
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

main :: IO ()
main = getProjectRootCurrent >>= \mpr -> case mpr of
    Nothing -> error "Couldn't find the project root"
    Just pr -> do
        fs <- getDirectoryContents pr
        case find ((".cabal" ==) . takeExtension) fs of
            Nothing -> error "Couldn't find your cabal file."
            Just fp -> hspecSetup pr (pr </> fp)

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

hspecSetup :: FilePath -> FilePath -> IO ()
hspecSetup pr fp = do
    c <- getCurrentDirectory

    putStrLn $ "Adding test-suite to " <> makeRelative c fp <> "..."
    cabalContents <- readFile fp

    when ("type: exitcode-stdio-1.0" `isInfixOf` cabalContents) $ do
        hPutStrLn stderr "File already has test-suite. Exiting..."
        exitFailure
    appendFile fp hspecTestSuite

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
