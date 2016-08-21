{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Test.Hspec.SetupSpec
  where

import           Control.Exception
import           "cryptohash" Crypto.Hash
import qualified Data.ByteString.Char8 as ByteString (pack)
import           Data.Maybe
import           Data.Monoid
import           Language.Dockerfile
import           System.Directory
import           System.Exit
import           System.Process

import           Test.Hspec

-- | An image with hspec-setup and an empty project
newProjectImage :: EDockerfileM ()
newProjectImage = do
    from "haskell:7.10.3"
    run "cabal update"

    add "./hspec-setup.cabal" "/hspec-setup/"
    workdir "/hspec-setup"
    run "cabal install --only-dependencies -j"

    add "." "/hspec-setup"
    run "cabal install -j"

    run "mkdir /app"
    workdir "/app"
    run "cabal init -m -n"
    run "echo 'main = print 10' > /app/Main.hs"
    run "touch /app/LICENSE"
    run "stack init"
    run "stack build"

-- | Builds the image defined in this EDockerfileM block by writting it to a
-- temporary file
dockerBuild :: EDockerfileM () -> IO String
dockerBuild img = do
    let imgPre = "hspec-setup-tests-autogen"
        imgStr = toDockerfileStr img
        imgHash = show (hash (ByteString.pack imgStr) :: Digest MD5)
        imgName = imgPre <> ":" <> imgHash
        imgFp = "./" <> imgPre <> "." <> imgHash <> ".dockerfile"
    mimg <- fmap (take 2) . listToMaybe . map words . lines <$>
            readCreateProcess (shell ("docker images | grep " <> imgPre)) ""
            :: IO (Maybe [String])

    -- print mimg
    -- print (Just [imgPre, imgHash])

    if mimg == Just [imgPre, imgHash]
        then return imgName
        else do
            bracket_
                (writeFile imgFp imgStr)
                (removeFile imgFp)
                (callCommand $ "docker build -t " <> imgName <> " -f " <> imgFp <> " .")
            return imgName

-- | Runs the image with the tag passed-in
dockerRun :: String -> IO ()
dockerRun imgName = callCommand $ "docker run --rm " <> imgName

withImage :: EDockerfileM () -> (String -> IO b) -> IO b
withImage img action = do
    callCommand $ unwords [ "docker images |"
                          , "grep hspec-setup-tests-autogen |"
                          , "awk '{print $3}' |"
                          , "tail -n +3 |"
                          , "while read p; do"
                          , "docker rmi -f $p;"
                          , "done"
                          ]
    bracket
        (dockerBuild img)
        (const (return ()))
        -- (\imgTag -> callCommand ("docker rmi -f " <> imgTag))
        action

spec :: Spec
spec = do
    describe "given an empty cabal project" $
        it "works" $ do
            let img = newProjectImage >> cmd "hspec-setup"
            withImage img $ \tag ->
                dockerRun tag

    describe "given an empty hpack project" $
        it "works" $ do
            let img = newProjectImage >> do
                    run "stack install hpack-convert"
                    run "hpack-convert"
                    run "rm *.cabal"
                    run "echo 'library:\\n  dependencies:\\n  - base' >> package.yaml"
                    run "cat package.yaml"
                    run "hspec-setup"
                    cmd "cat package.yaml | grep 'tests:'"
            withImage img $ \tag ->
                dockerRun tag
