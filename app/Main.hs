{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

-- Import our modules
import Bencode
import HttpReq
import Peer
import Torrent
import Tracker
import Types

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  Control.Monad.when (length args < 2) $ do
    putStrLn "Usage: your_program.sh <command> <args>"
    exitWith (ExitFailure 1)

  case args of
    ("decode" : encodedValue : _) -> do
      hPutStrLn stderr "Logs from your program will appear here!"
      let (decodedValue, _) = decodeBencodedValue (B.pack encodedValue)
      let jsonValue = encode decodedValue
      LB.putStr jsonValue
      putStr "\n"
    -- "info" command - display torrent file information
    ("info" : filePath : _) -> do
      contents <- B.readFile filePath
      case parseTorrentFile contents of
        Left err -> hPutStrLn stderr $ "Error parsing torrent: " ++ err
        Right torrent -> do
          putStrLn $ "Tracker URL: " ++ torrentTrackerUrl torrent
          putStrLn $ "Length: " ++ show (torrentFileLength torrent)
          putStrLn $ "Info Hash: " ++ torrentInfoHashHex torrent
          putStrLn $ "Piece Length: " ++ show (torrentPieceLength torrent)
          putStrLn "Piece Hashes:"
          mapM_ putStrLn (torrentPieceHashes torrent)
    ("handshake" : filePath : peerAddr : _) -> do
      contents <- B.readFile filePath
      case parseTorrentFile contents of
        Left err -> hPutStrLn stderr $ "Error parsing torrent: " ++ err
        Right torrent -> do
          -- Parse peer address (format: ip:port)
          let (host, portStr) = break (== ':') peerAddr
          let port = drop 1 portStr -- Remove the ':' character

          -- Perform handshake
          result <- performHandshake host port (torrentInfoHashRaw torrent)

          case result of
            Nothing -> hPutStrLn stderr "Handshake failed"
            Just handshake -> do
              putStrLn $ "Peer ID: " ++ toHex (hsPeerId handshake)
    -- "peers" command - discover peers from tracker
    ("peers" : filePath : _) -> do
      contents <- B.readFile filePath
      case parseTorrentFile contents of
        Left err -> hPutStrLn stderr $ "Error parsing torrent: " ++ err
        Right torrent -> do
          -- Build tracker request
          let trackerReq =
                TrackerRequest
                  { reqInfoHash = torrentInfoHashRaw torrent
                  , reqPeerId = "00112233445566778899" -- 20-byte peer ID
                  , reqPort = 6881
                  , reqUploaded = 0
                  , reqDownloaded = 0
                  , reqLeft = torrentFileLength torrent
                  , reqCompact = 1
                  }

          -- Build full tracker URL with query params
          let trackerUrl = buildTrackerUrl (torrentTrackerUrl torrent) trackerReq

          -- Make HTTP request to tracker
          responseBody <- makeHttpRequest trackerUrl

          -- Parse bencoded response
          let responseBS = LB.toStrict responseBody
          let (decodedResponse, _) = decodeBencodedValue responseBS

          -- Extract peers from response
          case decodedResponse of
            BDict respDict ->
              case lookup "peers" respDict of
                Just (BString peersData) -> do
                  let peerList = parsePeers peersData
                  mapM_ putStrLn peerList
                _ -> hPutStrLn stderr "Peers not found in tracker response"
            _ -> hPutStrLn stderr "Invalid tracker response"
    (command : _) ->
      putStrLn $ "Unknown command: " ++ command
