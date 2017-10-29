module Posts
  ( countWords
  , extractFrontmatter
  , extractPostBody
  , isMarkdown
  , splitFrontMatterAndBody
  , findPosts
  ) where

import           System.Directory
import           System.FilePath
import           Data.List as List
import           Data.List.Split
import           Data.Char
import           Data.Map as Map
import           Data.Maybe

findPosts :: FilePath -> IO [FilePath]
findPosts = getDirectoryContents

isMarkdown :: FilePath -> Bool
isMarkdown fp = takeExtension fp == ".md"

splitFrontMatterAndBody :: String -> (String, String)
splitFrontMatterAndBody = toTuple . splitPost
  where
    splitPost = List.map trim . List.filter notEmpty . splitOn "+++"
    toTuple [] = ("", "")
    toTuple [x] = (x, "")
    toTuple (x:xs) = (x, unwords xs)

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

countWords :: String -> Map String Int
countWords = List.foldr (count . toLowerCase) Map.empty . words
  where
    count word memo = Map.insert word (currentCount word memo + 1) memo
    currentCount word memo = fromMaybe 0 $ Map.lookup word memo

toLowerCase :: String -> String
toLowerCase = List.map toLower
