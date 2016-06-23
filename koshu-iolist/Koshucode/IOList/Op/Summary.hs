{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Summary operations.

module Koshucode.IOList.Op.Summary
 ( opFind, opSummary, opGrand,
 ) where

import qualified Koshucode.Baala.Base               as K
import qualified Koshucode.IOList.File              as K
import qualified Koshucode.IOList.Output            as K
import qualified Koshucode.IOList.Parts             as K
import qualified Koshucode.IOList.Op.Run            as K


-- ----------------------  find

-- | @find@ -- Find I/O list scripts.
opFind :: K.Op
opFind _ scripts =
  do K.forFilesRec_ "." K.dirActionVisible p
     return K.StatusMessage
  where
    p f | isScript     = putStrLn $ K.slash $ K.fileDirs f
        | otherwise    = return ()
        where isScript = K.fileName f `elem` scripts


-- ----------------------  summary

-- | @summary@ -- Create summary of I/O lists.
opSummary :: K.Op
opSummary p@K.Param {..} scripts =
    do K.putHead '-' summaryTitle
       ss <- K.forFilesUp "." action
       let cnt = K.statusCount ss
       putStrLn ""
       save summaryTotal summaryTitle paramSummary cnt ss
       case paramSummary of
         Just s  -> return $ K.StatusSummary s cnt ss
         Nothing -> return $ K.StatusMessage
    where
      summaryTitle = "Summary of I/O Lists"
      summaryTotal = "Total"

      action :: K.DirAction ([K.Status], [FilePath])
      action up files = do
        let files' = K.omitHidden files
        ms <- K.getCommandScript scripts up files'
        case ms of
          Nothing -> return ([], files')
          Just  s -> do putFile " - " $ K.scriptFile s
                        st <- K.try $ K.runScriptContent p s
                        return $ ([st], files')

putFile :: String -> K.FileDirs -> IO ()
putFile prefix f = do
  putStr prefix
  putStrLn $ K.slashSpace $ K.fileDirs f


-- ----------------------  grand summary

-- | @grand@ -- Create grand summary.
opGrand :: K.Op
opGrand p@K.Param {..} scripts =
    do K.putHead '=' grandTitle
       ss <- K.forFilesUp "." action
       let cnt = K.statusCount ss
       save grandTotal grandTitle paramGrand cnt ss
       case paramGrand of
         Just s  -> return $ K.StatusGrand s ss
         Nothing -> return $ K.StatusMessage
    where
      grandTitle = "Grand Summary of I/O Lists"
      grandTotal = "Grand total"

      action :: K.DirAction ([K.Status], [FilePath])
      action up files = do
        let files' = K.omitHidden files
        ms <- K.getSummaryScript scripts up files'
        case ms of
          Nothing  -> return ([], files')
          Just s   -> do st <- K.runScriptContent p s
                         putStrLn ""
                         return ([st], [])


-- --------------------------------------------  common parts

save :: String -> String -> Maybe K.FileDirs -> K.StatusCount -> [K.Status] -> IO ()
save total title script cnt ss =
  saveFile total script cnt $
    K.mdHead 1 title
    K.<> mconcat (K.mdFileItem <$> K.mdFileDirs <$> K.statusFiles ss)

saveFile :: String -> Maybe K.FileDirs -> K.StatusCount -> K.MixText -> IO ()
saveFile total (Nothing) cnt _ = putStrLn $ countText total cnt
saveFile total (Just script) cnt doc =
  do putStr    $ countText total cnt
     putStr    $ " on "
     putStrLn  $ K.slashSpace $ K.fileDirs script
     K.writeMix K.crlfBreak (K.mdFilePath script) doc

countText :: String -> K.StatusCount -> String
countText total cnt =
    total ++ " "         ++ show (K.statusTotal  cnt)
          ++ ", update " ++ show (K.statusUpdate cnt)
          ++ ", skip "   ++ show (K.statusSkip   cnt)

