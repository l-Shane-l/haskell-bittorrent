{-# LANGUAGE OverloadedStrings #-}

module Peer where

import qualified Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)
import Network.Simple.TCP (connect, recv, send)
import System.Random (randomIO)
import Text.Printf (printf)

-- Handshake data type
data Handshake = Handshake
  { hsInfoHash :: BS.ByteString -- 20 bytes
  , hsPeerId :: BS.ByteString -- 20 bytes
  }
  deriving (Show)

-- Generate a random 20-byte peer ID
generatePeerId :: IO BS.ByteString
generatePeerId = BS.pack <$> Control.Monad.replicateM 20 randomIO

-- Serialize handshake to ByteString
serializeHandshake :: Handshake -> BS.ByteString
serializeHandshake (Handshake infoHash peerId) =
  BS.concat
    [ BS.singleton 19 -- length of protocol string
    , B.pack "BitTorrent protocol" -- protocol string (19 bytes)
    , BS.replicate 8 0 -- 8 reserved bytes
    , infoHash -- 20 bytes
    , peerId -- 20 bytes
    ]

-- Parse handshake from ByteString
parseHandshake :: BS.ByteString -> Maybe Handshake
parseHandshake bs
  | BS.length bs < 68 = Nothing -- Minimum handshake size
  | otherwise =
      let pstrlen = BS.index bs 0
          expectedLen = 49 + fromIntegral pstrlen -- 1 + pstrlen + 8 + 20 + 20
       in if BS.length bs < expectedLen
            then Nothing
            else
              let offset = 1 + fromIntegral pstrlen + 8 -- Skip pstrlen, protocol, reserved
                  infoHash = BS.take 20 (BS.drop offset bs)
                  peerId = BS.take 20 (BS.drop (offset + 20) bs)
               in Just $ Handshake infoHash peerId

-- Convert ByteString to hex string
toHex :: BS.ByteString -> String
toHex = concatMap (printf "%02x") . BS.unpack

-- Perform handshake with a peer
performHandshake :: String -> String -> BS.ByteString -> IO (Maybe Handshake)
performHandshake host port infoHash = do
  -- Generate random peer ID
  peerId <- generatePeerId

  -- Create handshake message
  let handshake = Handshake infoHash peerId
  let handshakeMsg = serializeHandshake handshake

  -- Connect to peer and exchange handshake
  connect host port $ \(socket, _) -> do
    -- Send handshake
    send socket handshakeMsg

    -- Receive handshake response (68 bytes minimum)
    maybeResponse <- recv socket 68
    case maybeResponse of
      Nothing -> return Nothing
      Just response -> return $ parseHandshake response
