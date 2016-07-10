module System.Directory.Recursive
  where

import           System.Directory      (getDirectoryContents)
import           System.Directory.Tree

getDirectoryContentsRecursive
  :: FilePath -> IO (AnchoredDirTree String)
getDirectoryContentsRecursive = readDirectory

recursiveLink = undefined

recursiveMirror = undefined
