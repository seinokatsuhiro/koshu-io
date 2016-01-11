{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.Assoc
 ( Assoc (..),
   assoc,
   assocLookup,
   assocPrefix,
 ) where

import qualified Data.List as List

data Assoc a = Assoc
    { assocKey    :: String
    , assocValue  :: a
    }

instance Show (Assoc a) where
    show (Assoc n _) = n

instance Eq (Assoc a) where
    (Assoc n1 _) == (Assoc n2 _) = n1 == n2

instance Ord (Assoc a) where
    compare (Assoc n1 _) (Assoc n2 _) = compare n1 n2

assoc :: String -> a -> Assoc a
assoc n a = Assoc n a

assocLookup :: String -> [Assoc a] -> Maybe a
assocLookup n = loop where
    loop [] = Nothing
    loop ((Assoc n' a) : ops)
        | n == n'    = Just a
        | otherwise  = loop ops

assocPrefix :: String -> [Assoc a] -> [Assoc a]
assocPrefix n = loop [] where
    loop ys [] = reverse ys
    loop ys (op@(Assoc n' _) : xs)
        | n == n'                 = [op]
        | n `List.isPrefixOf` n'  = loop (op : ys) xs
        | otherwise               = loop ys xs

