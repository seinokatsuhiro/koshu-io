{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Param
 ( Param (..),
   Operation,
   param,
   paramYmd,
   paramSetGrand,
   paramSetSummary,
   paramSetScript,
   mdFileDirs,
   mdFilePath,
 ) where

import qualified Data.Dates                      as D
import qualified System.Environment              as Env

import qualified Koshucode.IOList.Operate        as K
import qualified Koshucode.IOList.Utility        as K


-- --------------------------------------------  Param

data Param = Param
  { paramProg           :: String               -- ^ Program name
  , paramArgs           :: [String]             -- ^ Command-line arguments
  , paramOperations     :: [K.Assoc Operation]  -- ^ Operations
  , paramDateTime       :: Maybe D.DateTime     -- ^ Invocation date-time
  , paramTitle          :: Maybe String         -- ^ Title of I/O list
  , paramAuthor         :: Maybe String         -- ^ Author of I/O list
  , paramGrand          :: Maybe K.FileDirs     -- ^ Grand summary script
  , paramSummary        :: Maybe K.FileDirs     -- ^ Summary script
  , paramScript         :: Maybe K.FileDirs     -- ^ I/O script
  , paramOutput         :: FilePath             -- ^ Name of output markdown file
  } deriving (Show, Eq, Ord)

instance K.GetOperations Param where
    getOperations = paramOperations

param :: IO Param
param = do
  prog <- Env.getProgName
  args <- Env.getArgs
  date <- D.getCurrentDateTime
  return Param { paramProg          = prog
               , paramArgs          = args
               , paramOperations    = []
               , paramDateTime      = Just date
               , paramTitle         = Just "I/O List"
               , paramAuthor        = Just "koshu-io"
               , paramGrand         = Nothing
               , paramSummary       = Nothing
               , paramScript        = Nothing
               , paramOutput        = "IOLIST.md"
               }
                 
paramYmd :: Param -> Maybe String
paramYmd Param {..} = fmap paramYmd' paramDateTime

paramYmd' :: D.DateTime -> String
paramYmd' dt = ymd where
    ymd = show y ++ "-" ++ show m ++ "-" ++ show d
    y   = D.year  dt
    m   = D.month dt
    d   = D.day   dt

paramSetGrand :: Param -> K.FileDirs -> Param
paramSetGrand p f = p { paramGrand = Just f }

paramSetSummary :: Param -> K.FileDirs -> Param
paramSetSummary p f = p { paramSummary = Just f }

paramSetScript :: Param -> K.FileDirs -> Param
paramSetScript p f =
    p { paramScript   = Just f
      , paramOutput   = mdFilePath f }

mdFileDirs :: K.FileDirs -> K.FileDirs
mdFileDirs = K.changeExtension "md"

mdFilePath :: K.FileDirs -> FilePath
mdFilePath = K.fileName . mdFileDirs


-- --------------------------------------------  Operation

type Operation = K.Operation' Param
--type Operation' param = param -> Maybe FilePath -> [String] -> IO K.Status

-- operate :: Operation
-- operate _ _ [] = help
-- operate p file (name : args) =
--     case K.assocPrefix name $ paramOperations p of
--       [K.Assoc _ op] -> op p file args
--       []             -> help
--       ops            -> ambiguous $ map K.assocKey ops

-- ambiguous :: [String] -> IO K.Status
-- ambiguous ns = do
--   putStrLn "MESSAGE"
--   putStrLn "  Ambiguous operation"
--   putStrLn ""
--   putStrLn "DETAIL"
--   mapM_ putName ns
--   putStrLn ""
--   return K.StatusMessage

-- putName :: String -> IO ()
-- putName n = do
--   putStr "  "
--   putStrLn n

-- help :: IO K.Status
-- help = do
--   putStrLn "DESCRIPTION"
--   putStrLn "  Generate and compare I/O list"
--   putStrLn ""
--   putStrLn "USAGE"
--   putStrLn "  koshu-io OPERATION"
--   putStrLn ""
--   putStrLn "OPERATION"
--   putStrLn "  command       CMD ARG ..."
--   putStrLn "  command       CMD ARG ... // FILE ..."
--   putStrLn "  find          IOLIST ..."
--   putStrLn "  grand-init    IOLIST"
--   putStrLn "  run           IOLIST"
--   putStrLn "  summary-init  IOLIST"
--   putStrLn ""
--   putStrLn "OPERATION in IOLIST script"
--   putStrLn "  grand         IOLIST ..."
--   putStrLn "  summary       IOLIST ..."
--   putStrLn ""
--   return K.StatusMessage
