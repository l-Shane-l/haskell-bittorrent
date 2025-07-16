{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad
import Data.Aeson (ToJSON (..), encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

data BencodedValue = BString String | BInteger Integer | BList [BencodedValue]
  deriving (Show, Eq)

instance ToJSON BencodedValue where
  toJSON (BString str) = toJSON str
  toJSON (BInteger num) = toJSON num
  toJSON (BList list) = toJSON list

byteStringToBInteger :: ByteString -> BencodedValue
byteStringToBInteger numberBS = BInteger (read (B.unpack numberBS))

byteStringToBString :: ByteString -> BencodedValue
byteStringToBString stringBS = BString (B.unpack stringBS)

byteStringToInt :: ByteString -> Int
byteStringToInt stringInt = read (B.unpack stringInt) :: Int

decodeList :: ByteString -> [BencodedValue]
decodeList bs
  | B.null bs = []
  | otherwise =
      let (firstItem, remainder) = decodeBencodedValue bs
          restOfTheItems = decodeList remainder
       in firstItem : restOfTheItems

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
      let
        listContent = B.init $ B.tail encodedValue
        decodedItems = decodeList listContent
        result = BList decodedItems
        remainder = B.empty
       in
        (result, remainder)
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
    -- If the first arg is "decode" and a second arg exists...
    ("decode" : encodedValue : _) -> do
      hPutStrLn stderr "Logs from your program will appear here!"
      let (decodedValue, _) = decodeBencodedValue (B.pack encodedValue)
      let jsonValue = encode decodedValue
      LB.putStr jsonValue
      putStr "\n"

    -- For any other single command...
    (command : _) ->
      putStrLn $ "Unknown command: " ++ command
