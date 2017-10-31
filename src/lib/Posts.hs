module Posts
  ( countWords
  , extractFrontmatter
  , extractPostBody
  , isMarkdown
  , splitFrontMatterAndBody
  , findPosts
  ) where

import           Control.Monad    (join)
import           Data.Bifunctor   (bimap)
import           Data.Char
import           Data.List        as List
import           Data.Map         as Map
import           Data.Maybe
import           System.Directory
import           System.FilePath

findPosts :: FilePath -> IO [FilePath]
findPosts = getDirectoryContents

isMarkdown :: FilePath -> Bool
isMarkdown fp = takeExtension fp == ".md"

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith x y = x == take (length x) y

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

takeBetween :: Int -> Int -> [a] -> [a]
takeBetween start end = slice (start + 1) (end - 1)

joinLines :: [String] -> String
joinLines = intercalate "\n"

splitFrontMatterAndBody :: String -> (String, String)
splitFrontMatterAndBody s = mapToStrings $ toTuple (indices asLines) asLines
  where
    asLines = lines s
    indices = findIndices (startsWith "+++")
    toTuple (x:y:_) ls = (takeBetween x y ls, drop (y + 1) ls)
    toTuple _ ls       = (ls, [])
    mapToStrings = join bimap joinLines

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
