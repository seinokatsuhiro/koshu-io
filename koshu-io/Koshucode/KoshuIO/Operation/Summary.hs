{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Operation.Summary
 ( opFind, opSummary, opGrand,
 ) where

import qualified Control.Monad                       as M
import qualified Data.Maybe                          as Maybe
import qualified Data.List                           as List

import qualified Koshucode.KoshuIO.Param             as K
import qualified Koshucode.KoshuIO.Status            as K
import qualified Koshucode.KoshuIO.Utility           as K
import qualified Koshucode.KoshuIO.Operation.Run     as K


-- ----------------------  find

-- | Operation for @find@
opFind :: K.Operation
opFind _ scripts =
  do K.forFiles_ K.dirActionVisible p "."
     return K.StatusMessage
  where
    p f | isScript     = putStrLn $ K.slash $ K.fileDirs f
        | otherwise    = return ()
        where isScript = K.fileName f `elem` scripts


-- ----------------------  summary

-- | Operation for @summary@
opSummary :: K.Operation
opSummary param@K.Param {..} scripts =
    do K.putHead '-' summaryTitle
       ss <- K.forFiles K.dirActionVisible select "."
       let ss' = Maybe.catMaybes ss
           cnt = K.statusCount ss'
       putStrLn ""
       save summaryTotal summaryTitle paramSummary cnt ss'
       case paramSummary of
         Just s  -> return $ K.StatusSummary s cnt ss'
         Nothing -> return $ K.StatusMessage
    where
      summaryTitle :: String
      summaryTitle = "Summary of I/O Lists"

      summaryTotal :: String
      summaryTotal = "Total"

      select :: SummaryAction
      select f
          | dirs == ["."]        = return Nothing
          | file `elem` scripts  = run f
          | otherwise            = return Nothing
          where
            file   = K.fileName f
            dirs   = K.fileRevDirs f

      run :: SummaryAction
      run f = do
        putFile " - " f
        let param' = param { K.paramArgs = ["run", K.fileName f] }
        st <- K.try $ K.runScript param' f
        return $ Just st

putFile :: String -> K.FileDirs -> IO ()
putFile prefix f = do
  putStr prefix
  putStrLn $ K.slashSpace $ K.fileDirs f


-- ----------------------  grand summary

-- | Operation for @grand@
opGrand :: K.Operation
opGrand param@K.Param {..} scripts =
    do K.putHead '=' grandTitle
       ss <- K.forFiles check select "."
       let ss' = Maybe.catMaybes ss
           cnt = K.statusCount ss'
       save grandTotal grandTitle paramGrand cnt ss'
       case paramGrand of
         Just s  -> return $ K.StatusGrand s ss'
         Nothing -> return $ K.StatusMessage
    where
      grandTitle :: String
      grandTitle = "Grand Summary of I/O Lists"

      grandTotal :: String
      grandTotal = "Grand total"

      check :: K.DirAction
      check _ files = do
        let files' = K.omitHidden files
            fs     = List.intersect scripts files'
        fs' <- M.filterM isSummaryIO fs
        case fs' of
          [f]  -> return [f]
          _    -> return files'

      select :: SummaryAction
      select f
          | dirs == ["."]        = return Nothing
          | file `elem` scripts  = run f
          | otherwise            = return Nothing
          where
            file   = K.fileName f
            dirs   = K.fileRevDirs f

      run :: SummaryAction
      run f = do
        let file = K.fileName f
        cmds <- K.readCommand file
        case cmds of
          [cmd] | isSummary cmd -> do
               let param' = param { K.paramArgs = ["run", file] }
               st <- K.runScript param' f
               putStrLn ""
               return $ Just st
          _ -> return Nothing

isSummaryIO :: FilePath -> IO Bool
isSummaryIO path = do
  cmds <- K.readCommand path
  case cmds of
    [cmd] | isSummary cmd -> return True
    _                     -> return False

isSummary :: String -> Bool
isSummary = K.beginWith ["koshu-io", "summary"]


-- --------------------------------------------  common parts

type SummaryAction = K.FileAction (Maybe K.Status)

save :: String -> String -> Maybe K.FileDirs -> K.StatusCount -> [K.Status] -> IO ()
save total title script cnt ss = do
  let doc = unlines $ ("% " ++ title) : "" : map linkItem (K.statusFiles ss)
  saveFile script total cnt doc

linkItem :: K.FileDirs -> String
linkItem script = item where
    item = "- [" ++ K.slashSpace path ++ "](" ++ K.slash path ++ ")"
    path = K.cutDot $ reverse $ (K.mdFilePath script) : K.fileRevDirs script

saveFile :: Maybe K.FileDirs -> String -> K.StatusCount -> String -> IO ()
saveFile (Nothing) total cnt _ = putStrLn $ countText total cnt
saveFile (Just script) total cnt doc =
  do putStr    $ countText total cnt
     putStr    $ " on "
     putStrLn  $ K.slashSpace $ K.fileDirs script
     writeFile (K.mdFilePath script) doc

countText :: String -> K.StatusCount -> String
countText total cnt = s where
    s = total ++ " "     ++ show (K.statusTotal  cnt)
        ++ ", update "   ++ show (K.statusUpdate cnt)
        ++ ", skip "     ++ show (K.statusSkip   cnt)
          
