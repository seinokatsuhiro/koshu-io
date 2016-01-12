{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Operation.Summary
 ( opFind, opSummary, opGrand,
 ) where

import qualified Control.Monad                       as M
import qualified Data.Binary.Put                     as Put
import qualified Data.ByteString.Lazy                as Bz
import qualified Data.Maybe                          as Maybe
import qualified Data.List                           as List

import qualified Koshucode.KoshuIO.IOList            as K
import qualified Koshucode.KoshuIO.Param             as K
import qualified Koshucode.KoshuIO.Status            as K
import qualified Koshucode.KoshuIO.Utility           as K
import qualified Koshucode.KoshuIO.Operation.Run     as K


-- ----------------------  find

-- | Operation for @find@
opFind :: K.Operation
opFind _ scripts =
  do K.forFilesRec_ K.dirActionVisible p "."
     return K.StatusMessage
  where
    p f | isScript     = putStrLn $ K.slash $ K.fileDirs f
        | otherwise    = return ()
        where isScript = K.fileName f `elem` scripts


-- ----------------------  summary

-- | Operation for @summary@
opSummary :: K.Operation
opSummary p@K.Param {..} scripts =
    do K.putHead '-' summaryTitle
       ss <- K.forFilesRec K.dirActionVisible (run `for` scripts) "."
       let ss' = Maybe.catMaybes ss
           cnt = K.statusCount ss'
       putStrLn ""
       save summaryTotal summaryTitle paramSummary cnt ss'
       case paramSummary of
         Just s  -> return $ K.StatusSummary s cnt ss'
         Nothing -> return $ K.StatusMessage
    where
      summaryTitle = "Summary of I/O Lists"
      summaryTotal = "Total"

      run :: SummaryAction
      run f = do
        putFile " - " f
        st <- K.try $ K.runScript p f
        return $ Just st

for :: SummaryAction -> [FilePath] -> SummaryAction
for run scripts f
    | dirs == ["."]        = return Nothing
    | file `elem` scripts  = run f
    | otherwise            = return Nothing
    where file = K.fileName f
          dirs = K.fileRevDirs f

putFile :: String -> K.FileDirs -> IO ()
putFile prefix f = do
  putStr prefix
  putStrLn $ K.slashSpace $ K.fileDirs f


-- ----------------------  grand summary

opGrand :: K.Operation
opGrand p@K.Param {..} scripts =
    do K.putHead '=' grandTitle
       ss <- K.forFilesRec check (run `for` scripts) "."
       let ss' = Maybe.catMaybes ss
           cnt = K.statusCount ss'
       save grandTotal grandTitle paramGrand cnt ss'
       case paramGrand of
         Just s  -> return $ K.StatusGrand s ss'
         Nothing -> return $ K.StatusMessage
    where
      grandTitle = "Grand Summary of I/O Lists"
      grandTotal = "Grand total"

      check :: K.DirFilter
      check _ files = do
        let files' = K.omitHidden files
            fs     = List.intersect scripts files'
        fs' <- M.filterM isSummaryIO fs
        case fs' of
          [f]  -> return [f]
          _    -> return files'

      run :: SummaryAction
      run f = do
        cmds <- K.readScript $ K.fileName f
        case cmds of
          [cmd] | isSummary cmd -> do
               st <- K.runScript p f
               putStrLn ""
               return $ Just st
          _ -> return Nothing

isSummaryIO :: FilePath -> IO Bool
isSummaryIO path = do
  cmds <- K.readScript path
  case cmds of
    [cmd] | isSummary cmd -> return True
    _                     -> return False

isSummary :: String -> Bool
isSummary = K.beginWith ["koshu-io", "summary"]


-- --------------------------------------------  common parts

type SummaryAction = K.FileAction (Maybe K.Status)

save :: String -> String -> Maybe K.FileDirs -> K.StatusCount -> [K.Status] -> IO ()
save total title script cnt ss =
  saveFile total script cnt $ do
    K.mdHead 1 title
    mapM_ K.mdFileItem $ K.mdFileDirs `map` K.statusFiles ss

saveFile :: String -> Maybe K.FileDirs -> K.StatusCount -> Put.Put -> IO ()
saveFile total (Nothing) cnt _ = putStrLn $ countText total cnt
saveFile total (Just script) cnt doc =
  do putStr    $ countText total cnt
     putStr    $ " on "
     putStrLn  $ K.slashSpace $ K.fileDirs script
     Bz.writeFile (K.mdFilePath script) (Put.runPut doc)

countText :: String -> K.StatusCount -> String
countText total cnt =
    total ++ " "         ++ show (K.statusTotal  cnt)
          ++ ", update " ++ show (K.statusUpdate cnt)
          ++ ", skip "   ++ show (K.statusSkip   cnt)

