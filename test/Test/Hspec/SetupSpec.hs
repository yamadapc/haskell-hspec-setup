{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.SetupSpec
  where

import           Crypto.Hash
import qualified Data.ByteString.Char8 as ByteString (pack)
import           Data.Monoid
import           Language.Dockerfile
import           System.Directory
import           System.Exit
import           System.Process

import           Test.Hspec

-- | An image with hspec-setup and an empty project
newProjectImage :: EDockerfileM ()
newProjectImage = do
    from "haskell:8"
    add "." "/hspec-setup"
    run "stack install"

    run "mkdir /app"
    workdir "/app"
    run "cabal init --minimal"

-- | Builds the image defined in this EDockerfileM block by writting it to a
-- temporary file
dockerBuild :: EDockerfileM () -> IO String
dockerBuild img = do
    let imgStr = toDockerfileStr img
        imgHash = show (hash (ByteString.pack imgStr) :: Digest MD5)
        imgName = "hspec-setup-tests-autogen:" <> imgHash
        imgFp = "./hspec-setup-tests-autogen." <> imgHash <> ".dockerfile"
    writeFile imgFp imgStr
    callCommand $ "docker build -t " <> imgName <> " -f " <> imgFp <> " ."
    removeFile imgFp
    return imgName

-- | Runs the image with the tag passed-in
dockerRun :: String -> IO (ExitCode, String, String)
dockerRun imgName = readCreateProcessWithExitCode
    (shell $ "docker run -it --rm " <> imgName) ""

spec :: Spec
spec = do
    describe "given an empty cabal project" $ do
        it "works" $ do
            tag <- dockerBuild $ newProjectImage >> do
                cmd "hspec-setup"
            (ec, _, _) <- dockerRun tag
            ec `shouldBe` ExitSuccess
