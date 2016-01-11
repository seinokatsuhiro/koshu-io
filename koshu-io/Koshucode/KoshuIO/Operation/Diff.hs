{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Operation.Diff
 ( Line, diff, report,
 ) where

import qualified Data.Algorithm.Diff                 as Diff
import qualified Data.ByteString.Lazy                as Bz
import qualified Data.ByteString.Lazy.Char8          as Bc


-- --------------------------------------------  diff

type Line = (Int, Bz.ByteString)      

diff :: Bz.ByteString -> Bz.ByteString -> [[Bz.ByteString]]
diff bz1 bz2 =
    let ls1 = diffLine bz1
        ls2 = diffLine bz2
    in report $ Diff.getGroupedDiffBy compareSnd ls1 ls2

compareSnd :: Eq a => (x, a) -> (y, a) -> Bool
compareSnd x y = snd x == snd y

diffLine :: Bz.ByteString -> [Line]
diffLine bz = zip [1..] $ Bc.split '\n' bz


-- --------------------------------------------  report

report :: [Diff.Diff [Line]] -> [[Bz.ByteString]]
report [] = []
report (Diff.First x  : Diff.Second y :  xs)
                            = changed x y : report xs
report (Diff.First x  : xs) = deleted x   : report xs
report (Diff.Second x : xs) = added x     : report xs
report (Diff.Both _ _ : xs) =               report xs

changed :: [Line] -> [Line] -> [Bz.ByteString]
changed del add = "Changed " : deladd where
    deladd = map (liner " - ") del
          ++ map (liner " + ") add

deleted :: [Line] -> [Bz.ByteString]
deleted del = "Deleted " : map (liner " - ") del

added :: [Line] -> [Bz.ByteString]
added add = "Added " : map (liner " + ") add

liner :: Bz.ByteString -> Line -> Bz.ByteString
liner prefix (n, bz) = prefix `Bz.append` Bc.pack (lineNumber n)
                              `Bz.append` bz

lineNumber :: Int -> String
lineNumber n = take 5 (show n ++ repeat ' ') ++ " "

