{-# OPTIONS_GHC -Wall #-}

-- | Association list.

module Koshucode.IOList.Parts.Assoc
 ( Assoc (..),
   assoc,
   assocLookup,
   assocPrefix,
 ) where

import qualified Data.List as List

-- | Association of string and something.
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

-- | Create assoc.
assoc :: String -> a -> Assoc a
assoc = Assoc

-- | Find assoc by key.
assocLookup :: String -> [Assoc a] -> Maybe a
assocLookup n = loop where
    loop [] = Nothing
    loop ((Assoc n' a) : ops)
        | n == n'    = Just a
        | otherwise  = loop ops

-- | Find assocs by prefix matching.
assocPrefix :: String -> [Assoc a] -> [Assoc a]
assocPrefix n = loop [] where
    loop ys [] = reverse ys
    loop ys (op@(Assoc n' _) : xs)
        | n == n'                 = [op]
        | n `List.isPrefixOf` n'  = loop (op : ys) xs
        | otherwise               = loop ys xs

