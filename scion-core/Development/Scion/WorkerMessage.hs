{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification,
    StandaloneDeriving, DeriveGeneric, OverloadedStrings
 #-}
module Development.Scion.WorkerMessage where

import Control.Applicative
import Data.Aeson
import Data.Int ( Int64 )
--import Data.Monoid
import GHC.Generics ( Generic )
--import System.FilePath
import System.IO -- ( Handle )
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC8

------------------------------------------------------------------------------

data WorkerCommand
  = GetWorkerVersion
  | InitWorker [T.Text]  -- GHC flags
  | ParseImports !FilePath
  deriving (Eq, Show, Generic)

instance ToJSON WorkerCommand
instance FromJSON WorkerCommand


data WorkerResponse
  = WorkerVersion ![Int]
  | GhcWorkerReady [T.Text] -- warnings
  deriving (Eq, Show, Generic)

instance ToJSON WorkerResponse
instance FromJSON WorkerResponse

------------------------------------------------------------------------------

encodeMessageLength :: Int64 -> BLC8.ByteString
encodeMessageLength len =
  BLC8.pack (replicate (8 - length lenText) '0' ++ lenText)
 where
   lenText = show len

sendMessageToHandle :: ToJSON a => Handle -> a -> IO ()
sendMessageToHandle hdl message = do
  --putStrLn $ "Sending message: " <> show lenString <> ", " <> show encodedMessage
  BLC8.hPut hdl lenString
  BLC8.hPut hdl encodedMessage
  hFlush hdl
 where
   encodedMessage = encode message
   lenString = encodeMessageLength (BLC8.length encodedMessage)

recvMessageFromHandle :: FromJSON a => Handle -> IO (Either String a)
recvMessageFromHandle hdl = do
  --putStrLn $ "Receiving message"
  --hFlush stdout
  len <- read . BLC8.unpack <$> BLC8.hGet hdl 8
  --putStrLn $ "Received length: " <> show len
  --hFlush stdout
  msg <- BLC8.hGet hdl len
  return $! eitherDecode msg
