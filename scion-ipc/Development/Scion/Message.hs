module Development.Scion.Message where

import Control.Applicative
import Control.Exception ( SomeException, try )
import Control.Monad ( when )
import Data.Binary
import Data.Int ( Int64 )
import Data.Maybe ( isJust )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import GHC.IO.Handle ( hDuplicateTo, hDuplicate )
import System.IO ( Handle, hFlush, hGetEncoding, hSetBinaryMode )

sendMessage :: Binary a => Handle -> a -> IO ()
sendMessage hdl message = do
  -- We send the encoded chunks prefixed by the length of each chunk.
  -- The message is terminated by a chunk of length -1.
  --
  -- If we just rely on "decode", we may end up waiting for input forever
  -- if the two sides don't agree on the encoding.
  sendChunks $ BL.toChunks $ encode message
 where
   sendChunks [] = do
     let len = encode ((-1) :: Int64)
     when (BL.length len /= 8) $ badEncoding
     BL.hPut hdl len
     hFlush hdl
   sendChunks (c:cs) = do
     let len = encode (fromIntegral (B.length c) :: Int64)
     when (BL.length len /= 8) $ badEncoding
     BL.hPut hdl len
     B.hPut hdl c
     sendChunks cs

   badEncoding =
     error $ unlines
       [ "Development.Scion.Message.sendMessage: binary encoding of"
       , "integers has changed. TODO: Use putWord8s" ]

recvMessage :: Binary a => Handle -> IO (Either String a)
recvMessage hdl = do
  -- TODO: We want an IOStream-like parser (ie., continuations for partial
  -- results)
  rslt <- try $ decode . BL.fromChunks <$> getChunks
  case rslt of
    Left e -> return $! Left $! show (e :: SomeException)
    Right a -> return $! Right a

 where
   getChunks = do
     len <- decode <$> BLC8.hGet hdl 8
     if (len :: Int64) >= 0 then do
       c <- B.hGet hdl (fromIntegral len)
       (c:) <$> getChunks
      else
       return []

------------------------------------------------------------------------------

-- | Ensure that the handle is in binary mode.
ensureBinaryMode :: Handle -> IO ()
ensureBinaryMode h = do
  enc <- hGetEncoding h
  when (isJust enc) $
    hSetBinaryMode h True

-- | Get exclusive access to the first handle's resource.
--
-- Subsequent writes to the first handle are redirected to the second
-- handle.  The returned handle is an exclusive handle to the resource
-- initially held by the first handle.
makeExclusive ::
     Handle -- ^ The handle to the resource that we want exclusive
            -- access to.
  -> Handle -- ^ Anything written to the original handle will be
            -- redirected to this one.
  -> IO Handle -- ^ The exclusive handle.
makeExclusive hexcl hredirect = do
  hFlush hexcl
  hFlush hredirect
  hresult <- hDuplicate hexcl
  hDuplicateTo hredirect hexcl
  return hresult
