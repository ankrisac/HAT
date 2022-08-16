{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BS

import           Data.Word
import           Encode

import           Control.Monad


data Target = Target Word8 Word8 
  deriving Show

elfMagic = "\x7F\&ELF"


enc :: Encoder m => Target -> m ()
enc (Target x y) = do
  putBytes elfMagic

  putU8 x
  putU8 y

dec :: Decoder m => m (Maybe Target)
dec = do
  magic  <- getBytes (BS.length elfMagic)
  width  <- getU8
  endian <- getU8

  pure $ if magic == elfMagic 
    then Just $ Target width endian
    else Nothing


main :: IO ()
main = do
  putStrLn "-- ELF --"

  let path = "out/file"

  BS.writeFile path (encodeLE $ enc head)
  BS.readFile path >>= print . decodeLE dec

  pure ()
  where
    head = Target 1 2
