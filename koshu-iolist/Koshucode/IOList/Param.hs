{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Param
 ( -- * Type
   Param (..),
   Op,

   -- * Function
   param,
   paramSetGrand,
   paramSetSummary,
   paramSetCommand,
   mdFileDirs,
   mdFilePath,
 ) where

import qualified Data.Dates                      as D
import qualified System.Environment              as Env

import qualified Koshucode.IOList.Operation      as K
import qualified Koshucode.IOList.Utility        as K


-- --------------------------------------------  Param

-- | Operation type for I/O list.
type Op = K.Operation Param

-- | Parameter for I/O list.
data Param = Param
  { paramProg           :: String               -- ^ Program name
  , paramArgs           :: [String]             -- ^ Command-line arguments
  , paramOps            :: [K.Assoc Op]         -- ^ Operations
  , paramDateTime       :: Maybe D.DateTime     -- ^ Invocation date-time
  , paramTitle          :: Maybe String         -- ^ Title of I/O list
  , paramAuthor         :: Maybe String         -- ^ Author of I/O list
  , paramGrand          :: Maybe K.FileDirs     -- ^ Grand summary script
  , paramSummary        :: Maybe K.FileDirs     -- ^ Summary script
  , paramCommand        :: Maybe K.FileDirs     -- ^ Command script
  , paramDefault        :: FilePath             -- ^ Default command script
  , paramOutput         :: FilePath             -- ^ Name of output markdown file
  } deriving (Show, Eq, Ord)

instance K.GetOperations Param where
    getOperations = paramOps

-- | Get parameter for current process.
param :: IO Param
param = do
  prog <- Env.getProgName
  args <- Env.getArgs
  date <- D.getCurrentDateTime
  return Param { paramProg          = prog
               , paramArgs          = args
               , paramOps           = []
               , paramDateTime      = Just date
               , paramTitle         = Just "I/O List"
               , paramAuthor        = Just "iolist"
               , paramGrand         = Nothing
               , paramSummary       = Nothing
               , paramCommand       = Nothing
               , paramDefault       = "IOLIST"
               , paramOutput        = "IOLIST.md"
               }
                 
-- paramYmd :: Param -> Maybe String
-- paramYmd Param {..} = fmap paramYmd' paramDateTime

-- paramYmd' :: D.DateTime -> String
-- paramYmd' dt = ymd where
--     ymd = show y ++ "-" ++ show m ++ "-" ++ show d
--     y   = D.year  dt
--     m   = D.month dt
--     d   = D.day   dt

-- | Set grand summary script.
paramSetGrand :: Param -> K.FileDirs -> Param
paramSetGrand p f = p { paramGrand = Just f }

-- | Set summary script.
paramSetSummary :: Param -> K.FileDirs -> Param
paramSetSummary p f = p { paramSummary = Just f }

-- | Set command script.
paramSetCommand :: Param -> K.FileDirs -> Param
paramSetCommand p f =
    p { paramCommand  = Just f
      , paramOutput   = mdFilePath f }

-- | Output markdown file.
mdFileDirs :: K.FileDirs -> K.FileDirs
mdFileDirs = K.changeExtension "md"

-- | Path of output markdown.
mdFilePath :: K.FileDirs -> FilePath
mdFilePath = K.fileName . mdFileDirs
