{-# LANGUAGE OverloadedStrings #-}

module Torrent 
  ( parseTorrentFile
  ) where

import Bencode
import Types
import Crypto.Hash (Digest, SHA1 (..), hash)
import Data.ByteArray (convert)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- Parse a torrent file into a TorrentInfo structure
parseTorrentFile :: ByteString -> Either String TorrentInfo
parseTorrentFile contents = do
  let (decodedValue, _) = decodeBencodedValue contents
  case decodedValue of
    BDict rootDict -> do
      -- Get tracker URL
      trackerUrl <- case lookup "announce" rootDict of
        Just (BString url) -> Right $ B.unpack url
        _ -> Left "Tracker URL not found"
      
      -- Get info dictionary
      infoDict <- case lookup "info" rootDict of
        Just d@(BDict _) -> Right d
        _ -> Left "Info dictionary not found"
      
      case infoDict of
        BDict innerDict -> do
          -- Get file length
          fileLength <- case lookup "length" innerDict of
            Just (BInteger len) -> Right len
            _ -> Left "Length not found"
          
          -- Calculate info hash (both raw and hex)
          let encodedInfo = encodeBencodedValue infoDict
          let infoHashDigest = hash encodedInfo :: Digest SHA1
          let infoHashHex = show infoHashDigest
          let infoHashRaw = convert infoHashDigest :: ByteString  -- Raw 20 bytes for tracker
          
          -- Get piece length
          pieceLen <- case lookup "piece length" innerDict of
            Just (BInteger pLen) -> Right pLen
            _ -> Left "Piece length not found"
          
          -- Get piece hashes
          pieceHashes <- case lookup "pieces" innerDict of
            Just (BString rawString) -> do
              let pieces = chunksOf 20 rawString
              Right $ fmap bytesToHex pieces
            _ -> Left "Pieces not found"
          
          -- Build the TorrentInfo
          return $ TorrentInfo
            { torrentTrackerUrl = trackerUrl
            , torrentFileLength = fileLength
            , torrentInfoHashRaw = infoHashRaw
            , torrentInfoHashHex = infoHashHex
            , torrentPieceLength = pieceLen
            , torrentPieceHashes = pieceHashes
            }
        _ -> Left "Info is not a dictionary"
    _ -> Left "Torrent file is not a valid dictionary"

