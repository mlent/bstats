module Posts 
  ( findPosts
  ) where

import System.Directory

findPosts :: FilePath -> IO [FilePath]
findPosts = getDirectoryContents
