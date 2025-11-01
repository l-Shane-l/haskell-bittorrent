{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (ToJSON (..), object)
import Data.Aeson.Key (fromString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- Bencoded value type
data BencodedValue
  = BString ByteString
  | BInteger Integer
  | BList [BencodedValue]
  | BDict [(ByteString, BencodedValue)]
  deriving (Show, Eq)

instance ToJSON BencodedValue where
  toJSON (BString str) = toJSON (B.unpack str)
  toJSON (BInteger num) = toJSON num
  toJSON (BList list) = toJSON list
  toJSON (BDict dict) = object [(fromString (B.unpack k), toJSON v) | (k, v) <- dict]

data PeerProtocol = PeerProtocol
  { lengthOfProtocol :: Int
  , bittorrentProtocol :: ByteString
  , reservedBytes :: ByteString
  , infoHash :: ByteString
  , peerId :: ByteString
  }
  deriving (Show)

-- Parsed torrent file
data TorrentInfo = TorrentInfo
  { torrentTrackerUrl :: String
  , torrentFileLength :: Integer
  , torrentInfoHashRaw :: ByteString -- Raw 20 bytes for tracker request
  , torrentInfoHashHex :: String -- Hex string for display
  , torrentPieceLength :: Integer
  , torrentPieceHashes :: [String] -- Hex encoded piece hashes
  }
  deriving (Show)

-- Tracker request type
data TrackerRequest = TrackerRequest
  { reqInfoHash :: ByteString
  , reqPeerId :: ByteString
  , reqPort :: Int
  , reqUploaded :: Integer
  , reqDownloaded :: Integer
  , reqLeft :: Integer
  , reqCompact :: Int
  }
  deriving (Show)

-- Tracker response type
data TrackerResponse = TrackerResponse
  { respInterval :: Int
  , respPeers :: ByteString
  }
  deriving (Show)
