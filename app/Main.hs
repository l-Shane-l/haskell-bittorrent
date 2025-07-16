{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

data BencodedValue = BString String | BInteger Integer
  deriving (Show, Eq)

decodeBencodedValue :: ByteString -> BencodedValue
decodeBencodedValue encodedValue
  | isDigit (B.head encodedValue) =
      case B.elemIndex ':' encodedValue of
        Just colonIndex -> BString (B.unpack (B.drop (colonIndex + 1) encodedValue))
        Nothing -> error "Invalid encoded value"
  | (B.head encodedValue == 'i') && (B.last encodedValue == 'e') =
      let numberPart = B.init (B.tail encodedValue)
       in BInteger (read (B.unpack numberPart))
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
      let decodedValue = decodeBencodedValue (B.pack encodedValue)
      let jsonValue = case decodedValue of
            BString str -> encode str
            BInteger num -> encode num
      LB.putStr jsonValue
      putStr "\n"

    -- For any other single command...
    (command : _) ->
      putStrLn $ "Unknown command: " ++ command
