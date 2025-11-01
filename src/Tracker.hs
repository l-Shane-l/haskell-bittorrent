{-# LANGUAGE OverloadedStrings #-}

module Tracker
  ( buildTrackerUrl
  , urlEncodeBytes
  , parsePeers
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)
import Text.Printf (printf)
import Types
import Bencode (chunksOf)

-- URL encode a ByteString (each byte becomes %XX)
urlEncodeBytes :: ByteString -> String
urlEncodeBytes bs = concatMap encodeChar (B.unpack bs)
  where
    encodeChar :: Char -> String
    encodeChar c = printf "%%%02x" (fromEnum c :: Int)

-- Build the full tracker URL with query parameters
buildTrackerUrl :: String -> TrackerRequest -> String
buildTrackerUrl baseUrl req =
  baseUrl ++ "?" ++ queryString
  where
    queryString = concat
      [ "info_hash=", urlEncodeBytes (reqInfoHash req)
      , "&peer_id=", urlEncodeBytes (reqPeerId req)
      , "&port=", show (reqPort req)
      , "&uploaded=", show (reqUploaded req)
      , "&downloaded=", show (reqDownloaded req)
      , "&left=", show (reqLeft req)
      , "&compact=", show (reqCompact req)
      ]

-- Parse peers from compact format (6 bytes per peer: 4 bytes IP + 2 bytes port)
parsePeers :: ByteString -> [String]
parsePeers peersData = map formatPeer (chunksOf 6 peersData)
  where
    formatPeer :: ByteString -> String
    formatPeer peerBytes
      | B.length peerBytes == 6 =
          let [b1, b2, b3, b4, p1, p2] = map (fromEnum :: Char -> Int) (B.unpack peerBytes)
              ip = printf "%d.%d.%d.%d" b1 b2 b3 b4 :: String
              port = p1 * 256 + p2 :: Int
          in printf "%s:%d" ip port :: String
      | otherwise = "Invalid peer data"

