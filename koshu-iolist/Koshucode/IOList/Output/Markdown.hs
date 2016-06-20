{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Output.Markdown
 ( ToMarkdown (..),
   mdTitle,
   mdHead,
   mdStatus,
   mdFileItem,
   mdBlock,
 ) where

import Data.Monoid ((<>))
import qualified Data.ByteString             as Bs
import qualified Data.ByteString.Char8       as Bc
import qualified Data.Char                   as Ch
import qualified System.Exit                 as Exit

import qualified Koshucode.Baala.Base        as K
import qualified Koshucode.IOList.File       as K
import qualified Koshucode.IOList.Parts      as K


-- ----------------------  ToMarkdown

class ToMarkdown a where
    toMarkdown :: a -> K.MixText

instance (ToMarkdown a) => ToMarkdown (Maybe a) where
    toMarkdown Nothing   = K.mixEmpty
    toMarkdown (Just a)  = toMarkdown a

instance (ToMarkdown a) => ToMarkdown [a] where
    toMarkdown = mconcat . map toMarkdown


-- ----------------------  Heading

-- | Create markdown title.
mdTitle :: K.Param -> K.MixText
mdTitle K.Param { K.paramTitle = Nothing } = K.mixEmpty
mdTitle K.Param { K.paramTitle = Just s }  = K.mixLine $ mdHead 1 s

-- | Create markdown heading.
mdHead :: Int -> String -> K.MixText
mdHead 1 = mdHeadPrefix "#"
mdHead 2 = mdHeadPrefix "##"
mdHead 3 = mdHeadPrefix "###"
mdHead _ = mdHeadPrefix "####"

mdHeadPrefix :: String -> String -> K.MixText
mdHeadPrefix p text =
    K.mixLine (K.mixString p <> K.mix1 <> K.mixString text)
     <> K.mixHard


-- ----------------------  Status

-- | Create exit status markdown segment.
mdStatus :: K.CmdLine -> Exit.ExitCode -> K.MixText
mdStatus cmdline exit =
    K.mixLine (emp cmdline <> K.mix1 <> mixExit exit <> K.mixBs ".")
     <> K.mixHard

emp :: String -> K.MixText
emp s = K.mixBracket ast ast $ K.mixString s where
    ast = K.mixBs "**"

mixExit :: Exit.ExitCode -> K.MixText
mixExit (Exit.ExitSuccess)   = K.mixBs "exits successfully"
mixExit (Exit.ExitFailure n) = K.mixBs "fails with status " <> K.mixShow n


-- ----------------------  Link

-- | Create linked file name.
mdFileItem :: K.FileDirs -> K.MixText
mdFileItem file = K.mixLine item where
    path = K.fileDirs file
    item = K.mixBs "- ["
           <> K.mixString (K.slashSpace path)
           <> K.mixBs "]("
           <> K.mixString (K.slash path)
           <> K.mixBs ")"


-- ----------------------  Block

-- | Create file content block.
mdBlock :: K.Bs -> K.MixText
mdBlock bs
    | Bs.null bs        = K.mixEmpty
    | Bc.all isText bs  = mdBlockText bs
    | otherwise         = mdBinary bs

isText :: Char -> Bool
isText c = Ch.isPrint c || Ch.isSpace c

mdBlockText :: K.Bs -> K.MixText
mdBlockText bs = K.mixLine $
    mixFence
    <> (mconcat $ mixBsLine <$> Bc.lines bs)
    <> mixFence

mixBsLine :: K.Bs -> K.MixText
mixBsLine bs = K.mixLine $ K.mixBs $ Bc.filter (/= '\r') bs

mixFence :: K.MixText
mixFence = K.mixLine $ K.mixBs "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

mdBinary :: K.Bs -> K.MixText
mdBinary bs = K.mixLine $
    K.mixBs "Binary data of "
    <> K.mixShow (Bs.length bs)
    <> K.mixBs " bytes"

