module Cli
  ( run
  ) where

import           Data.List()
import           Data.Maybe
import           Printer
import           System.Environment as Env
import           System.IO()

type Command = [String] -> IO ()

commandList :: [(String, Command)]
commandList = [("help", help)]

findCommand :: String -> Command
findCommand cmd = fromMaybe unknown $ lookup cmd commandList

unknown :: Command
unknown args = putStr $ nl (unwords output)
  where
    output = ["Unknown command: ", unwords args]

help :: Command
help = logCmd "help"

run :: IO ()
run = do
  (cmd:args) <- Env.getArgs
  findCommand cmd args

logCmd :: String -> Command
logCmd cmd args = putStr $ nl (unwords output)
  where
    output = ["calling", cmd, "with", unwords args]
