module Posts
  ( findPosts
  , isMarkdown
  , extractFrontmatter
  , extractPostBody
  , splitFrontMatterAndBody
  ) where

import           System.Directory
import           System.FilePath
import           Data.List
import           Data.List.Split
import           Data.Char

findPosts :: FilePath -> IO [FilePath]
findPosts = getDirectoryContents

isMarkdown :: FilePath -> Bool
isMarkdown fp = takeExtension fp == ".md"

splitFrontMatterAndBody :: String -> (String, String)
splitFrontMatterAndBody content = (frontmatter, body)
  where
    splitBody = map trim . filter notEmpty $ splitOn "+++" content
    frontmatter = head splitBody
    body = unwords $ tail splitBody

trim :: String -> String
trim = trimL . trimR

trimL :: String -> String
trimL = dropWhile isSpace

trimR :: String -> String
trimR = reverse . trimL . reverse

notEmpty :: String -> Bool
notEmpty str = str /= ""

extractFrontmatter :: String -> String
extractFrontmatter = fst . splitFrontMatterAndBody

extractPostBody :: String -> String
extractPostBody = snd . splitFrontMatterAndBody
