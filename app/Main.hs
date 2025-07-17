{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Aeson.Key (fromString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

data BencodedValue = BString String | BInteger Integer | BList [BencodedValue] | BDict [(String, BencodedValue)]
  deriving (Show, Eq)

instance ToJSON BencodedValue where
  toJSON (BString str) = toJSON str
  toJSON (BInteger num) = toJSON num
  toJSON (BList list) = toJSON list
  toJSON (BDict dict) = object [(fromString k, toJSON v) | (k, v) <- dict]

byteStringToBInteger :: ByteString -> BencodedValue
byteStringToBInteger numberBS = BInteger (read (B.unpack numberBS))

byteStringToBString :: ByteString -> BencodedValue
byteStringToBString stringBS = BString (B.unpack stringBS)

byteStringToInt :: ByteString -> Int
byteStringToInt stringInt = read (B.unpack stringInt) :: Int

-- The new, smarter decodeList
decodeList :: ByteString -> ([BencodedValue], ByteString)
decodeList bs
  | B.head bs == 'e' = ([], B.tail bs)
  | otherwise =
      let (firstItem, restOfString) = decodeBencodedValue bs
          (otherItems, finalRemainder) = decodeList restOfString
       in (firstItem : otherItems, finalRemainder)

decodeDict :: ByteString -> ([(String, BencodedValue)], ByteString)
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
    -- "info" command with the correct logic
    ("info" : filePath : _) -> do
      contents <- B.readFile filePath
      let (decodedValue, _) = decodeBencodedValue contents
      case decodedValue of
        BDict rootDict -> do
          case lookup "announce" rootDict of
            Just (BString url) -> putStrLn $ "Tracker URL: " ++ url
            _ -> hPutStrLn stderr "Tracker URL not found."

          -- Use lookup on the dictionary to find the "info" dictionary
          case lookup "info" rootDict of
            Just (BDict infoDict) ->
              -- Use lookup on the inner dictionary to find the value for the "length" key
              case lookup "length" infoDict of
                Just (BInteger len) -> putStrLn $ "Length: " ++ show len
                _ -> hPutStrLn stderr "Length not found in info."
            _ -> hPutStrLn stderr "Info dictionary not found."
        _ -> hPutStrLn stderr "Error: Torrent file is not a valid dictionary." -- For any other single command...
    (command : _) ->
      putStrLn $ "Unknown command: " ++ command
