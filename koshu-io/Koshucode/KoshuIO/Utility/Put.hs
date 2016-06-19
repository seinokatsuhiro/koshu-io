{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.Put
 ( puts,
   putEmpty,
   putBs, putS,
   putln, putlnBs, putlnS,
   putHead,
 ) where

import qualified Data.Binary.Put        as Put
import qualified Data.ByteString.Char8  as Bc

-- | Collect multiple puts.
puts :: [Put.Put] -> Put.Put
puts = foldr (>>) putEmpty

-- | Put nothing.
putEmpty :: Put.Put
putEmpty = putBs ""

-- | Put bytestring.
putBs :: Bc.ByteString -> Put.Put
putBs = Put.putByteString

-- | Put string.
putS :: String -> Put.Put
putS = Put.putByteString . Bc.pack

-- | Put newline character.
putln :: Put.Put
putln = putBs "\n"

-- | Put bytestring and newline.
putlnBs :: Bc.ByteString -> Put.Put
putlnBs s = putBs s >> putln

-- | Put string and newline.
putlnS :: String -> Put.Put
putlnS s = putS s >> putln

-- | Put string with text line.
putHead :: Char -> String -> IO ()
putHead c text = do
  putStrLn text
  putStrLn $ headLine c
  putStrLn ""

headLine :: Char -> String
headLine = replicate 70

