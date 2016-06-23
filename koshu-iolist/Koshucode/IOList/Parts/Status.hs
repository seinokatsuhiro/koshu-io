{-# OPTIONS_GHC -Wall #-}

-- | Exit status.

module Koshucode.IOList.Parts.Status
 ( Status (..),
   StatusResult (..),
   StatusCount (..),
   statusFiles,
   statusCount,
   command,
   try,
 ) where

import qualified Data.Maybe                  as Maybe
import qualified System.Exit                 as Exit
import qualified Koshucode.IOList.File       as K

-- | Exit status.
data Status
    = StatusMessage                                  -- ^ Exit on some messages
    | StatusCommand K.FileDirs StatusResult          -- ^ Command Script
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

-- | I/O list counter.
data StatusCount = StatusCount
    { statusTotal    :: Int    -- ^ Total number of I/O lists.
    , statusUpdate   :: Int    -- ^ Number of updated lists.
    , statusSkip     :: Int    -- ^ Number of skipped lists.
    } deriving (Show, Ord, Eq)

statusFiles :: [Status] -> [K.FileDirs]
statusFiles = Maybe.mapMaybe f where
    f (StatusCommand  file _)    = Just file
    f (StatusSummary  file _ _)  = Just file
    f (StatusGrand    file _)    = Just file
    f _                          = Nothing

-- | Extract and collect status results.
statusResults :: Status -> [StatusResult]
statusResults = loop where
    loop (StatusCommand  _ r)     = [r]
    loop (StatusSummary  _ _ ss)  = concatMap loop ss
    loop (StatusGrand    _ ss)    = concatMap loop ss
    loop _                        = []

-- | Count results.
statusCount :: [Status] -> StatusCount
statusCount ss = StatusCount total update skip where
    rs     = concatMap statusResults ss
    total  = length rs
    update = length $ filter (== StatusUpdate) rs
    skip   = length $ filter (== StatusSkip)   rs

command :: IO Status -> IO ()
command body =
    do _ <- try body
       Exit.exitWith Exit.ExitSuccess

try :: IO Status -> IO Status
try body =
    do ok <- body
       case isQuit ok of
         True  -> Exit.exitWith $ Exit.ExitFailure 1
         False -> return ok

isQuit :: Status -> Bool
isQuit = resultIs StatusQuit

resultIs :: StatusResult -> Status -> Bool
resultIs r (StatusCommand _ r') = r == r'
resultIs _ _ = False

