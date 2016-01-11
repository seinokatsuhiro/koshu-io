{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Status
 ( Status (..),
   StatusResult (..),
   StatusCount (..),
   statusFiles,
   statusCount,
   command,
   try,
 ) where

import qualified System.Exit                  as Exit
import qualified Koshucode.KoshuIO.Utility    as K

-- | Exit status.
data Status
    = StatusMessage                                  -- ^ Exit on some messages
    | StatusScript  K.FileDirs StatusResult          -- ^ I/O Script
    | StatusSummary K.FileDirs StatusCount [Status]  -- ^ Summary
    | StatusGrand   K.FileDirs [Status]              -- ^ Grand summary
      deriving (Show, Ord, Eq)

-- | Result of generating I/O list.
data StatusResult
    = StatusSave       -- ^ I/O list is saved
    | StatusUpdate     -- ^ I/O list is updated
    | StatusPass       -- ^ Pass regression test
    | StatusQuit       -- ^ Break regression test
    | StatusSkip       -- ^ Skip regression test
      deriving (Show, Ord, Eq)

data StatusCount = StatusCount
    { statusTotal    :: Int
    , statusUpdate   :: Int
    , statusSkip     :: Int
    } deriving (Show, Ord, Eq)

statusFiles :: [Status] -> [K.FileDirs]
statusFiles = concatMap f where
    f (StatusScript  file _)    = [file]
    f (StatusSummary file _ _)  = [file]
    f _                         = []

statusResults :: Status -> [StatusResult]
statusResults = loop where
    loop (StatusScript  _ r)     = [r]
    loop (StatusSummary _ _ ss)  = concatMap loop ss
    loop (StatusGrand   _ ss)    = concatMap loop ss
    loop _                       = []

statusCount :: [Status] -> StatusCount
statusCount ss = StatusCount total update skip where
    rs     = concatMap statusResults ss
    total  = length rs
    update = length $ filter (== StatusUpdate) rs
    skip   = length $ filter (== StatusSkip)   rs

resultIs :: StatusResult -> Status -> Bool
resultIs r (StatusScript _ r') = r == r'
resultIs _ _ = False

isQuit :: Status -> Bool
isQuit = resultIs StatusQuit

command :: IO Status -> IO ()
command action =
    do ok <- action
       case isQuit ok of
         True  -> Exit.exitWith $ Exit.ExitFailure 1
         False -> Exit.exitWith Exit.ExitSuccess

try :: IO Status -> IO Status
try action =
    do ok <- action
       case isQuit ok of
         True  -> Exit.exitWith $ Exit.ExitFailure 1
         False -> return ok

