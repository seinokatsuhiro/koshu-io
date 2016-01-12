{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.IOList.Markdown
 ( ToMarkdown (..),
   mdTitle,
   mdHead,
   mdStatus,
   mdFileItem,
   mdBlock,
 ) where

--import qualified Control.Monad                as C
import qualified Data.ByteString              as Bs
import qualified Data.ByteString.Char8        as Bc
import qualified Data.Binary.Put              as Put
import qualified Data.Char                    as Ch
import qualified System.Exit                  as Exit

import qualified Koshucode.KoshuIO.Param      as K
import qualified Koshucode.KoshuIO.Utility    as K


-- ----------------------  ToMarkdown

class ToMarkdown a where
    toMarkdown :: a -> Put.Put

instance (ToMarkdown a) => ToMarkdown (Maybe a) where
    toMarkdown Nothing   = K.putEmpty
    toMarkdown (Just a)  = toMarkdown a

instance (ToMarkdown a) => ToMarkdown [a] where
    toMarkdown = K.puts . map toMarkdown


-- ----------------------  Heading

mdTitle :: K.Param -> Put.Put
mdTitle K.Param { K.paramTitle = Nothing } = K.putEmpty
mdTitle K.Param { K.paramTitle = Just s }  = do
  mdHead 1 s
  K.putln

mdHead :: Int -> String -> Put.Put
mdHead 1 = mdHeadPrefix "# "
mdHead 2 = mdHeadPrefix "## "
mdHead 3 = mdHeadPrefix "### "
mdHead _ = mdHeadPrefix "#### "

mdHeadPrefix :: String -> String -> Put.Put
mdHeadPrefix p text = do
    K.putlnS $ p ++ text
    K.putln


-- ----------------------  Status

mdStatus :: K.CmdLine -> Exit.ExitCode -> Put.Put
mdStatus cmdline exit = do
  K.putlnS $ emp cmdline ++ " " ++ statusString exit ++ "."
  K.putln

emp :: String -> String
emp s = "**" ++ s ++ "**"

statusString :: Exit.ExitCode -> String
statusString (Exit.ExitSuccess)   = "exits successfully"
statusString (Exit.ExitFailure n) = "fails with status " ++ show n


-- ----------------------  Link

mdFileItem :: K.FileDirs -> Put.Put
mdFileItem file = K.putlnS item where
    item = "- [" ++ K.slashSpace path ++ "](" ++ K.slash path ++ ")"
    path = K.fileDirs file

-- ----------------------  Block

mdBlock :: Bs.ByteString -> Put.Put
mdBlock bs
    | Bs.null bs        = K.putEmpty
    | Bc.all isText bs  = mdBlockText bs
    | otherwise         = mdBinary bs

isText :: Char -> Bool
isText c = Ch.isPrint c || Ch.isSpace c

mdBlockText :: Bs.ByteString -> Put.Put
mdBlockText bs = do
  let ls = Bc.lines bs
  putFence
  mapM_ K.putlnBs ls
  -- C.when (Bc.last bs /= '\n') K.putln
  putFence
  K.putln

putFence :: Put.Put
putFence = K.putlnBs "~~~~~~~~~~"

mdBinary :: Bs.ByteString -> Put.Put
mdBinary bs =
    let n = show $ Bs.length bs
    in K.putlnS $ "Binary data of " ++ n ++ " bytes"

