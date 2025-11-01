{-# LANGUAGE OverloadedStrings #-}

module Bencode 
  ( encodeBencodedValue
  , decodeBencodedValue
  , bytesToHex
  , chunksOf
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (sortOn)
import Text.Printf (printf)
import Types

-- Helper functions
byteStringToBInteger :: ByteString -> BencodedValue
byteStringToBInteger numberBS = BInteger (read (B.unpack numberBS))

byteStringToBString :: ByteString -> BencodedValue
byteStringToBString = BString

byteStringToInt :: ByteString -> Int
byteStringToInt stringInt = read (B.unpack stringInt) :: Int

-- Encoding functions to convert BencodedValue back to bencode format
encodeBencodedValue :: BencodedValue -> ByteString
encodeBencodedValue (BString str) =
  let bs = str
      len = B.length bs
   in B.concat [B.pack (show len), ":", bs]
encodeBencodedValue (BInteger num) = B.concat ["i", B.pack (show num), "e"]
encodeBencodedValue (BList list) =
  B.concat $ ["l"] ++ map encodeBencodedValue list ++ ["e"]
encodeBencodedValue (BDict dict) =
  let sortedDict = sortOn fst dict -- Dictionary keys must be sorted in bencode
      encodePair (k, v) = B.concat [encodeBencodedValue (BString k), encodeBencodedValue v]
   in B.concat $ ["d"] ++ map encodePair sortedDict ++ ["e"]

-- Function to convert ByteString to hex string
bytesToHex :: ByteString -> String
bytesToHex bs = B.unpack bs >>= \c -> printf "%02x" c

-- Decoding functions
decodeList :: ByteString -> ([BencodedValue], ByteString)
decodeList bs
  | B.head bs == 'e' = ([], B.tail bs)
  | otherwise =
      let (firstItem, restOfString) = decodeBencodedValue bs
          (otherItems, finalRemainder) = decodeList restOfString
       in (firstItem : otherItems, finalRemainder)

decodeDict :: ByteString -> ([(ByteString, BencodedValue)], ByteString)
decodeDict bs
  | B.head bs == 'e' = ([], B.tail bs)
  | otherwise =
      let (BString key, restOfString_afterKey) = decodeBencodedValue bs
          (value, restofString_afterValue) = decodeBencodedValue restOfString_afterKey
          (otherPairs, finalRemainder) = decodeDict restofString_afterValue
       in ((key, value) : otherPairs, finalRemainder)

decodeBencodedValue :: ByteString -> (BencodedValue, ByteString)
decodeBencodedValue encodedValue
  | isDigit (B.head encodedValue) =
      case B.elemIndex ':' encodedValue of
        Just colonIndex ->
          let
            lenStr = B.take colonIndex encodedValue
            len = byteStringToInt lenStr
            afterColon = B.drop (colonIndex + 1) encodedValue
            content = B.take len afterColon
            remainder = B.drop len afterColon
           in
            (byteStringToBString content, remainder)
        Nothing -> error "Invalid encoded value"
  | B.head encodedValue == 'i' =
      case B.elemIndex 'e' encodedValue of
        Nothing -> error "Invalid bencoded integer: missing 'e'"
        Just eIndex ->
          let
            (integerToken, remainder) = B.splitAt (eIndex + 1) encodedValue
            numberPart = B.init (B.tail integerToken)
            result = byteStringToBInteger numberPart
           in
            (result, remainder)
  | B.head encodedValue == 'l' =
      let (items, remainder) = decodeList (B.tail encodedValue)
       in (BList items, remainder)
  | B.head encodedValue == 'd' =
      let (pairs, remainder) = decodeDict (B.tail encodedValue)
       in (BDict pairs, remainder)
  | otherwise = error $ "Unhandled encoded value: " ++ B.unpack encodedValue

-- Utility function to split ByteString into chunks
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf chunkSize longString
  | B.null longString = []
  | B.length longString <= chunkSize = [longString]
  | otherwise = B.take chunkSize longString : chunksOf chunkSize (B.drop chunkSize longString)

