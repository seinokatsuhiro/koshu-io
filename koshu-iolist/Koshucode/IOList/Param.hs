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
               , paramAuthor        = Just "iolist"
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

