{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Operation.Regress
 ( saveOrRegress,
   save, regress,
 ) where

import qualified Data.ByteString.Lazy               as Bz
import qualified System.Directory                   as Dir
import qualified System.IO                          as IO
import qualified Koshucode.Baala.Base               as K

import qualified Koshucode.IOList.Param             as K
import qualified Koshucode.IOList.Status            as K
import qualified Koshucode.IOList.Utility           as K
import qualified Koshucode.IOList.Operation.Diff    as K


-- --------------------------------------------  Save I/O list

-- | Save I/O list at first time, do regression test at second time.
saveOrRegress :: K.Param -> K.Bz -> IO K.Status -> IO K.Status
saveOrRegress p@K.Param {..} bz retry = do
  exist <- Dir.doesFileExist paramOutput
  case exist of
    False  -> save p bz            -- first time
    True   -> regress p bz retry   -- second time

-- | Save I/O list into markdown file.
save :: K.Param -> K.Bz -> IO K.Status
save p = saveWith (status p K.StatusSave) p

update :: K.Param -> K.Bz -> IO K.Status
update p bz = do putStrLn ""
                 saveWith (status p K.StatusUpdate) p bz

saveWith :: K.Status -> K.Param -> K.Bz -> IO K.Status
saveWith st K.Param {..} bz = do
  Bz.writeFile paramOutput bz
  return st


-- --------------------------------------------  Regression test

-- | Do regression test.
regress :: K.Param -> K.Bz -> IO K.Status -> IO K.Status
regress p@K.Param {..} bz2 retry = do
  bz1 <- Bz.readFile paramOutput
  case K.diff bz1 bz2 of
    []  -> return $ status p K.StatusPass
    rep -> do bzPutln ""
              K.putHead '*' "Differences of I/O list"
              mapM_ (mapM_ bzPutln) rep
              bzPutln ""
              prompt p bz2 retry

prompt :: K.Param -> K.Bz -> IO K.Status -> IO K.Status
prompt p bz2 retry = loop where
    loop = do
      putStr "Type [retry] [update] [skip] [exit] or [help]: "
      IO.hFlush IO.stdout
      res <- getLine
      case K.assocPrefix res operations of
        [K.Assoc _ op] -> op
        _              -> help >> loop

    operations =
        [ K.assoc "retry"  $ retry
        , K.assoc "update" $ update p bz2
        , K.assoc "skip"   $ skip p
        , K.assoc "exit"   $ exit p
        , K.assoc ""       $ loop
        ]

skip :: K.Param -> IO K.Status
skip p = do
  putStrLn ""
  return $ status p K.StatusSkip

exit :: K.Param -> IO K.Status
exit p = do
  putStrLn ""
  putStrLn "Exit on differences"
  return $ status p K.StatusQuit

status :: K.Param -> K.StatusResult -> K.Status
status K.Param { paramScript = script } =
    case script of
      Just s  -> K.StatusScript s
      Nothing -> error "No script"

help :: IO ()
help = do
  putStrLn ""
  putStrLn "  retry     retry this I/O list"
  putStrLn "  update    update this I/O list because differences are right"
  putStrLn "  skip      skip this I/O list"
  putStrLn "  quit      quit this command"
  putStrLn "  "

bzPutln :: K.Bz -> IO ()
bzPutln s = do
  Bz.hPut IO.stdout s
  Bz.hPut IO.stdout "\n"
