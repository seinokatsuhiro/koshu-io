{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Diff utility.

module Koshucode.IOList.Op.Diff
 ( Line, diff, report,
 ) where

import qualified Data.Algorithm.Diff                 as Diff
import qualified Data.ByteString.Lazy                as Bz
import qualified Data.ByteString.Lazy.Char8          as Bc
import qualified Koshucode.Baala.Base                as K


-- --------------------------------------------  diff

-- | Line number and content.
type Line = (Int, K.Bz)

-- | Diff of two lazy bytestrings.
diff :: K.Bz -> K.Bz -> [[K.Bz]]
diff bz1 bz2 =
    let ls1 = diffLine bz1
        ls2 = diffLine bz2
    in report $ Diff.getGroupedDiffBy compareSnd ls1 ls2

compareSnd :: Eq a => (x, a) -> (y, a) -> Bool
compareSnd x y = snd x == snd y

diffLine :: K.Bz -> [Line]
diffLine bz = zip [1..] $ Bc.split '\n' bz


-- --------------------------------------------  report

-- | Create diff report.
report :: [Diff.Diff [Line]] -> [[K.Bz]]
report [] = []
report (Diff.First x  : Diff.Second y :  xs)
                            = changed x y : report xs
report (Diff.First x  : xs) = deleted x   : report xs
report (Diff.Second x : xs) = added x     : report xs
report (Diff.Both _ _ : xs) =               report xs

changed :: [Line] -> [Line] -> [K.Bz]
changed del add = "Changed " : deladd where
    deladd = map (liner " - ") del
          ++ map (liner " + ") add

deleted :: [Line] -> [K.Bz]
deleted del = "Deleted " : map (liner " - ") del

added :: [Line] -> [K.Bz]
added add = "Added " : map (liner " + ") add

liner :: K.Bz -> Line -> K.Bz
liner prefix (n, bz) = prefix `Bz.append` Bc.pack (lineNumber n)
                              `Bz.append` bz

lineNumber :: Int -> String
lineNumber n = take 5 (show n ++ repeat ' ') ++ " "

