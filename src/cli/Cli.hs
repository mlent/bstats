module Cli
  ( run
  ) where

import           Data.List
import           Data.Maybe
import           Posts
import           Printer
import           System.Environment as Env
import           System.IO          ()

type Command = [String] -> IO ()

run :: IO ()
run = do
  (cmd:args) <- Env.getArgs
  findCommand cmd args

commandList :: [(String, Command)]
commandList = [("help", help), ("posts", posts)]

findCommand :: String -> Command
findCommand cmd = fromMaybe unknown $ lookup cmd commandList

unknown :: Command
unknown args = putStr $ nl (unwords output)
  where
    output = ["Unknown command: ", unwords args]

help :: Command
help = logCmd "help"

posts :: Command
posts [] = do
  let root = "/opt/life/content/post/travel"
  fileNames <- findPosts root
  content <- readFile $ root ++ "/" ++ (extractFilePath . findFirstPost) fileNames
  print $ extractFrontmatter content
  print $ extractPostBody content
  return ()

findFirstPost :: [FilePath] -> Maybe FilePath
findFirstPost = find isMarkdown

extractFilePath :: Maybe FilePath -> FilePath
extractFilePath = fromMaybe ""

logCmd :: String -> Command
logCmd cmd args = putStr $ nl (unwords output)
  where
    output = ["calling", cmd, "with", unwords args]
