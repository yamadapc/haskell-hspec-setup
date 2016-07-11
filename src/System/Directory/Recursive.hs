module System.Directory.Recursive
    (
      getDirectoryContentsRecursive
    , isHaskellSource
    , pretty
    , putDoc
    , module System.Directory.Tree
    )
  where

import           Debug.Trace
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen ((<$>))
-- import           Text.PrettyPrint.HughesPJClass

(<++>) = (Text.PrettyPrint.ANSI.Leijen.<$>)

getDirectoryContentsRecursive
  :: FilePath -> IO (AnchoredDirTree ())
getDirectoryContentsRecursive = readDirectoryWith (const $ return ())

instance Pretty (DirTree a) where
    pretty (Dir dn ds) = cyan (text dn) <$$>
        vcat (map (hang 2 . (text (replicate 2 ' ') <>) . pretty) ds)
    pretty (File fn _) = white (text fn) <> red (text " <-")
    pretty (Failed fn e) = red (text fn <> text (show e))

isHaskellSource (File fn _) =
    takeExtension fn == ".hs"
isHaskellSource (Dir _ []) = False
isHaskellSource (Dir ".stack-work" _) = False
isHaskellSource (Dir _ ds) = -- traceShow (ds, any isHaskellSource ds) $
    any isHaskellSource ds
isHaskellSource _ = False

recursiveLink = undefined

recursiveMirror = undefined
