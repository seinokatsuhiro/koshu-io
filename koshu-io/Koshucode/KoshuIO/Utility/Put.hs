{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.Put
 ( puts,
   putBs, putS,
   putlnBs, putlnS,
   putln, putEmpty,
   putHead,
 ) where

import qualified Data.Binary.Put        as Put
import qualified Data.ByteString.Char8  as Bc

puts :: [Put.Put] -> Put.Put
puts = foldr (>>) putEmpty

putBs :: Bc.ByteString -> Put.Put
putBs = Put.putByteString

putS :: String -> Put.Put
putS = Put.putByteString . Bc.pack

putlnBs :: Bc.ByteString -> Put.Put
putlnBs s = putBs s >> putln

putlnS :: String -> Put.Put
putlnS s = putS s >> putln

putln :: Put.Put
putln = putBs "\n"

putEmpty :: Put.Put
putEmpty = putBs ""

putHead :: Char -> String -> IO ()
putHead c text = do
  putStrLn text
  putStrLn $ headLine c
  putStrLn ""

headLine :: Char -> String
headLine = replicate 70
