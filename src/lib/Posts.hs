module Posts
  ( findPosts
  , isMarkdown
  ) where

import           System.Directory
import           System.FilePath

findPosts :: FilePath -> IO [FilePath]
findPosts = getDirectoryContents

isMarkdown :: FilePath -> Bool
isMarkdown fp = takeExtension fp == ".md"
