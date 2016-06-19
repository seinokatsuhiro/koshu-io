{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Utility.Put
 ( putHead,
 ) where

-- | Put string with text line.
putHead :: Char -> String -> IO ()
putHead c text = do
  putStrLn text
  putStrLn $ headLine c
  putStrLn ""

headLine :: Char -> String
headLine = replicate 70

