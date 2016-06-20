{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Utility.Script
 ( CmdLine, CmdName, CmdArg,
   Script (..),
   scriptFileDirs,
   getCommandScript,
   getSummaryScript,
   readScript,
 ) where

import qualified Data.List              as List
import qualified Data.Char              as Ch
import qualified System.Directory       as Dir

import qualified Koshucode.IOList.File  as K

-- | Command-line string
type CmdLine = String

-- | Name of program
type CmdName = String

-- | Command-line argument
type CmdArg = String

data Script
    = CommandScript K.FileDirs [String]  -- ^ Command script
    | SummaryScript K.FileDirs String    -- ^ Summary script
    | GrandScript   K.FileDirs String    -- ^ Grand summary script
      deriving (Show, Eq, Ord)

scriptFileDirs :: Script -> K.FileDirs
scriptFileDirs (CommandScript fd _) = fd
scriptFileDirs (SummaryScript fd _) = fd
scriptFileDirs (GrandScript   fd _) = fd

getCommandScript :: [FilePath] -> K.FileDirs -> [FilePath] -> IO (Maybe Script)
getCommandScript scripts up files =
  do r <- getScript scripts up files
     case r of
       Just (CommandScript _ _) -> return r
       _ -> return Nothing

getSummaryScript :: [FilePath] -> K.FileDirs -> [FilePath] -> IO (Maybe Script)
getSummaryScript scripts up files =
  do r <- getScript scripts up files
     case r of
       Just (SummaryScript _ _) -> return r
       _ -> return Nothing

getScript :: [FilePath] -> K.FileDirs -> [FilePath] -> IO (Maybe Script)
getScript scripts up files =
    case List.intersect scripts files of
      [file] -> do script <- readScript up file
                   return $ Just script
      _      -> return Nothing

-- | Read list of commands from file.
readScript :: K.FileDirs -> FilePath -> IO Script
readScript up path = do
  exist <- Dir.doesFileExist path
  case exist of
    False  -> error $ path ++ ": No such file or directory"
    True   -> do s <- readFile path
                 let fd = K.filePush path up
                 return $ case commands $ lines s of
                   [cmd] | beginWith ["iolist", "grand"]   cmd -> GrandScript   fd cmd
                         | beginWith ["iolist", "summary"] cmd -> SummaryScript fd cmd
                   cmds -> CommandScript fd cmds
    
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

