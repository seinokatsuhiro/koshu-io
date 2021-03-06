{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Section in I/O list.

module Koshucode.IOList.Output.Section
 ( -- * Section
   Section (..), section, subsection,
   sectionUp, sectionDeepen,
   -- * Numbering
   Numbering (..), numberingRoot,
 ) where

import qualified Koshucode.Baala.Base               as K
import qualified Koshucode.IOList.Output.Markdown   as K


-- ----------------------  Section

-- | Section.
data Section a = Section
    { sectionLevel  :: Int
    , sectionNumber :: [Int]
    , sectionTitle  :: String
    , sectionBody   :: a
    } deriving (Show, Eq, Ord)

-- | Create section.
section :: String -> a -> Section a
section = Section 2 []

-- | Create subsection.
subsection :: String -> a -> Section a
subsection = Section 3 []

-- | Count up section number.
sectionUp :: [Int] -> [Int]
sectionUp []     = []
sectionUp (n:ns) = n + 1 : ns

-- | Add section level.
sectionDeepen :: [Int] -> [Int]
sectionDeepen ns  = 1 : ns


-- ----------------------  Numbering

class Numbering a where
    numbering :: ([Int], a) -> ([Int], a)

instance (Numbering a) => Numbering (Maybe a) where
    numbering (ns, Nothing) = (ns, Nothing)
    numbering (ns, Just x)  = case numbering (ns, x) of
                                (ns', x') -> (ns', Just x')

instance (Numbering a) => Numbering [a] where
    numbering (ns0, xs0)  = loop ns0 [] xs0 where
        loop ns ys []     = (ns, reverse ys)
        loop ns ys (x:xs) = case numbering (ns, x) of
                              (ns', x') -> loop ns' (x' : ys) xs

instance (Numbering a) => Numbering (Section a) where
    numbering (nns, Section lv _ s a) =
        case numbering (nns, a) of
          (_, a') -> (sectionUp nns, Section lv (reverse nns) s a')

numberingRoot :: (Numbering a) => a -> a
numberingRoot a = snd $ numbering ([], a)


-- ----------------------  ToMarkdown

instance (K.ToMarkdown a) => K.ToMarkdown (Section a) where
    toMarkdown Section {..} =
      sectionNumberText sectionLevel sectionNumber sectionTitle
      K.<> K.toMarkdown sectionBody

sectionNumberText :: Int -> [Int] -> String -> K.MixText
sectionNumberText lv []    text  = K.mdHead lv text
sectionNumberText lv [a]   text  = K.mdHead lv $ show a ++ ". " ++ text
sectionNumberText lv [a,b] text  = K.mdHead lv $ show a ++ "." ++ show b ++ " " ++ text
sectionNumberText lv _     text  = K.mdHead lv text

