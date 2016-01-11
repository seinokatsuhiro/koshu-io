{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.Command
 ( CmdLine, CmdName, CmdArg,
   readCommand,
   beginWith,
 ) where

import qualified Data.List         as List
import qualified Data.Char         as Ch
import qualified System.Directory  as Dir

-- | Command-line string
type CmdLine = String

-- | Name of program
type CmdName = String

-- | Command-line argument
type CmdArg = String

-- | Read list of commands from file.
readCommand :: FilePath -> IO [String]
readCommand path = do
  exist <- Dir.doesFileExist path
  case exist of
    False  -> error $ path ++ ": No such file or directory"
    True   -> do s <- readFile path
                 return $ commands $ lines s
    
commands :: [String] -> [String]
commands = collect where
    collect []   = []
    collect [x]  = [x]
    collect (x : xs)
        | ignore x     = collect xs
        | otherwise   = case continue xs of
                          (cs, xs') -> unwords (x:cs) : collect xs'

    continue [] = ([], [])
    continue (c : xs)
        | ignore c    = continue xs
        | indented c  = case continue xs of
                          (cs, xs') -> (trim c : cs, xs')
        | otherwise   = ([], c : xs)

ignore :: String -> Bool
ignore s = case trim s of
             ""       -> True
             '#' : _  -> True
             _        -> False

trim :: String -> String
trim = dropWhile Ch.isSpace

indented :: String -> Bool
indented (c : _)   = Ch.isSpace c
indented _         = False

beginWith :: [String] -> String -> Bool
beginWith prefix cmdline =
    case words cmdline of
      args -> prefix `List.isPrefixOf` args

