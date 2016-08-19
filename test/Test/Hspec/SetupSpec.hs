module Test.Hspec.SetupSpec
  where

import           Language.Dockerfile
import           System.Process

import           Test.Hspec

newProjectImage :: EDockerfileM ()
newProjectImage = do
    from "haskell:8"
    run "mkdir /app"
    workdir "/app"
    run "cabal init --minimal"

spec :: Spec
spec =
    describe "given an empty cabal project" $ do
        undefined
