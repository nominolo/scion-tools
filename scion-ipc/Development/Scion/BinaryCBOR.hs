------------------------------------------------------------------------------
-- CURRENTLY UNUSED
--
-- Some experiments to encode and decode CBOR
-- (http://tools.ietf.org/html/rfc7049)
------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, BangPatterns, OverloadedStrings, MagicHash,
             ExistentialQuantification, PatternGuards #-}
module Development.Scion.BinaryCBOR where

import           Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LBI
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as BI
import           Data.Char (ord)
import           Data.Word
import           Data.Int
import           Data.Bits ((.|.), (.&.), xor, shiftR, shiftL)
import           GHC.Prim (Addr#)
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Foreign.Marshal.Utils    (copyBytes)
import           Foreign (touchForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr)
import           Foreign.Storable (poke)

import           Debug.Trace

------------------------------------------------------------------------------

data BufferRange =
  BufferRange {-# UNPACK #-} !(Ptr Word8) -- first byte of range
              {-# UNPACK #-} !(Ptr Word8) -- first byte /after/ range

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !BufferRange

-- | Allocate a new buffer of the given size.
{-# INLINE newBuffer #-}
newBuffer :: Int -> IO Buffer
newBuffer size = do
  fpbuf <- BI.mallocByteString size
  let pbuf = unsafeForeignPtrToPtr fpbuf
  return $! Buffer fpbuf (BufferRange pbuf (pbuf `plusPtr` size))

-- | Convert the filled part of a 'Buffer' to a strict 'B.ByteString'.
{-# INLINE byteStringFromBuffer #-}
byteStringFromBuffer :: Buffer -> B.ByteString
byteStringFromBuffer (Buffer fpbuf (BufferRange op _)) =
    BI.PS fpbuf 0 (op `minusPtr` unsafeForeignPtrToPtr fpbuf)

------------------------------------------------------------------------------

-- | A stream of chunks that are constructed in the 'IO' monad.
data ChunkIOStream a
  = Finished Buffer a
    -- ^ The partially filled last buffer together with the result.
  | Yield1 B.ByteString (IO (ChunkIOStream a))
    -- ^ Yield a /non-empty/ strict 'B.ByteString'.

-- | A smart constructor for yielding one chunk that ignores the chunk if
-- it is empty.
{-# INLINE yield1 #-}
yield1 :: B.ByteString -> IO (ChunkIOStream a) -> IO (ChunkIOStream a)
yield1 bs cios | B.null bs = cios
               | otherwise = return $ Yield1 bs cios

printChunkIOStream :: ChunkIOStream a -> IO ()
printChunkIOStream c = case c of
  Finished buf _ -> do let bs' = byteStringFromBuffer buf
                       putStrLn $ show (B.unpack bs')

  Yield1 bs m    -> do putStrLn (show (B.unpack bs))
                       c' <- m
                       printChunkIOStream c'

test1 = do
  iostr <- buildStepToCIOS (safeStrategy 20 20) $ encodeOutStream str
  printChunkIOStream iostr
 where
   str = [OWord64 12, OWord64 50, OWord64 700, OWord64 1243213
         ,OInt64 12, OInt64 (-20), OInt64 1000000000000]

------------------------------------------------------------------------------

-- | A buffer allocation strategy for executing 'Builder's.

-- The strategy
--
-- > 'AllocationStrategy' firstBufSize bufSize trim
--
-- states that the first buffer is of size @firstBufSize@, all following buffers
-- are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- be trimmed iff @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy
         (Maybe (Buffer, Int) -> IO Buffer)
         {-# UNPACK #-} !Int
         (Int -> Int -> Bool)

-- | Create a custom allocation strategy. See the code for 'safeStrategy' and
-- 'untrimmedStrategy' for examples.
{-# INLINE customStrategy #-}
customStrategy
  :: (Maybe (Buffer, Int) -> IO Buffer)
     -- ^ Buffer allocation function. If 'Nothing' is given, then a new first
     -- buffer should be allocated. If @'Just' (oldBuf, minSize)@ is given,
     -- then a buffer with minimal size 'minSize' must be returned. The
     -- strategy may reuse the 'oldBuffer', if it can guarantee that this
     -- referentially transparent and 'oldBuffer' is large enough.
  -> Int
     -- ^ Default buffer size.
  -> (Int -> Int -> Bool)
     -- ^ A predicate @trim used allocated@ returning 'True', if the buffer
     -- should be trimmed before it is returned.
  -> AllocationStrategy
customStrategy = AllocationStrategy

-- | Sanitize a buffer size; i.e., make it at least the size of an 'Int'.
{-# INLINE sanitize #-}
sanitize :: Int -> Int
sanitize = max 8 --(sizeOf (undefined :: Int))

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
{-# INLINE untrimmedStrategy #-}
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk
untrimmedStrategy firstSize bufSize =
    AllocationStrategy nextBuffer (sanitize bufSize) (\_ _ -> False)
  where
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer $ sanitize firstSize
    nextBuffer (Just (_, minSize)) = newBuffer minSize


-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- likely to survive one garbage collection. This strategy trims buffers
-- that are filled less than half in order to avoid spilling too much memory.
{-# INLINE safeStrategy #-}
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize =
    AllocationStrategy nextBuffer (sanitize bufSize) trim
  where
    trim used size                 = 2 * used < size
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer $ sanitize firstSize
    nextBuffer (Just (_, minSize)) = newBuffer minSize

------------------------------------------------------------------------------

type BuildStep a = BufferRange -> IO (BuildSignal a)

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- three signals: 'done', 'bufferFull', or 'insertChunks signals
data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     (BuildStep a)
  | InsertChunk
      {-# UNPACK #-} !(Ptr Word8)
                     B.ByteString
                     (BuildStep a)

------------------------------------------------------------------------------

-- | Convert a 'BuildStep' to a 'ChunkIOStream' stream by executing it on
-- 'Buffer's allocated according to the given 'AllocationStrategy'.
{-# INLINE buildStepToCIOS #-}
buildStepToCIOS
    :: AllocationStrategy          -- ^ Buffer allocation strategy to use
    -> BuildStep a                 -- ^ 'BuildStep' to execute
    -> IO (ChunkIOStream a)
buildStepToCIOS !(AllocationStrategy nextBuffer bufSize trim) =
    \step -> nextBuffer Nothing >>= fill step
  where
    fill !step !buf@(Buffer fpbuf br@(BufferRange _ pe)) = do
        res <- fillWithBuildStep step doneH fullH insertChunkH br
        touchForeignPtr fpbuf
        return res
      where
        pbuf = unsafeForeignPtrToPtr fpbuf

        doneH op' x = return $
            Finished (Buffer fpbuf (BufferRange op' pe)) x

        fullH op' minSize nextStep =
            wrapChunk op' $ const $
                nextBuffer (Just (buf, max minSize bufSize)) >>= fill nextStep

        insertChunkH op' bs nextStep =
            wrapChunk op' $ \isEmpty -> yield1 bs $
                -- Checking for empty case avoids allocating 'n-1' empty
                -- buffers for 'n' insertChunkH right after each other.
                if isEmpty
                  then fill nextStep buf
                  else do buf' <- nextBuffer (Just (buf, bufSize))
                          fill nextStep buf'

        -- Wrap and yield a chunk, trimming it if necesary
        {-# INLINE wrapChunk #-}
        wrapChunk !op' mkCIOS
          | chunkSize == 0      = mkCIOS True
          | trim chunkSize size = do
              bs <- BI.create chunkSize $ \pbuf' ->
                        copyBytes pbuf' pbuf chunkSize
              -- FIXME: We could reuse the trimmed buffer here.
              return $ Yield1 bs (mkCIOS False)
          | otherwise            =
              return $ Yield1 (BI.PS fpbuf 0 chunkSize) (mkCIOS False)
          where
            chunkSize = op' `minusPtr` pbuf
            size      = pe  `minusPtr` pbuf

-- | Fill a 'BufferRange' using a 'BuildStep'.
{-# INLINE fillWithBuildStep #-}
fillWithBuildStep
    :: BuildStep a
    -- ^ Build step to use for filling the 'BufferRange'.
    -> (Ptr Word8 -> a -> IO b)
    -- ^ Handling the 'done' signal
    -> (Ptr Word8 -> Int -> BuildStep a -> IO b)
    -- ^ Handling the 'bufferFull' signal
    -> (Ptr Word8 -> B.ByteString -> BuildStep a -> IO b)
    -- ^ Handling the 'insertChunk' signal
    -> BufferRange
    -- ^ Buffer range to fill.
    -> IO b
    -- ^ Value computed while filling this 'BufferRange'.
fillWithBuildStep step fDone fFull fChunk !br = do
    signal <- step br
    case signal of
        Done op x                      -> fDone op x
        BufferFull minSize op nextStep -> fFull op minSize nextStep
        InsertChunk op bs nextStep     -> fChunk op bs nextStep


{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.ByteString
       -- ^ Lazy 'L.ByteString' to use as the tail of the generated lazy
       -- 'L.ByteString'
    -> Builder
       -- ^ 'Builder' to execute
    -> L.ByteString
       -- ^ Resulting lazy 'L.ByteString'
toLazyByteStringWith strategy k b =
    ciosUnitToLazyByteString strategy k $ unsafeDupablePerformIO $
        buildStepToCIOS strategy (runBuilder b)

-- | Convert a @'ChunkIOStream' ()@ to a lazy 'L.ByteString' using
-- 'unsafeDupablePerformIO'.
{-# INLINE ciosUnitToLazyByteString #-}
ciosUnitToLazyByteString :: AllocationStrategy
                         -> L.ByteString -> ChunkIOStream () -> L.ByteString
ciosUnitToLazyByteString strategy k = go
  where
    go (Finished buf _) = trimmedChunkFromBuffer strategy buf k
    go (Yield1 bs io)   = L.Chunk bs $ unsafeDupablePerformIO (go <$> io)

------------------------------------------------------------------------------

-- | 'Builder's denote sequences of bytes.
-- They are 'Monoid's where
--   'mempty' is the zero-length sequence and
--   'mappend' is concatenation, which runs in /O(1)/.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- | Construct a 'Builder'. In contrast to 'BuildStep's, 'Builder's are
-- referentially transparent.
{-# INLINE builder #-}
builder :: (forall r. BuildStep r -> BuildStep r)
        -- ^ A function that fills a 'BufferRange', calls the continuation with
        -- the updated 'BufferRange' once its done, and signals its caller how
        -- to proceed using 'done', 'bufferFull', or 'insertChunk'.
        --
        -- This function must be referentially transparent; i.e., calling it
        -- multiple times with equally sized 'BufferRange's must result in the
        -- same sequence of bytes being written. If you need mutable state,
        -- then you must allocate it anew upon each call of this function.
        -- Moroever, this function must call the continuation once its done.
        -- Otherwise, concatenation of 'Builder's does not work. Finally, this
        -- function must write to all bytes that it claims it has written.
        -- Otherwise, the resulting 'Builder' is not guaranteed to be
        -- referentially transparent and sensitive data might leak.
        -> Builder
builder = Builder

toLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

toLazyByteString :: 

------------------------------------------------------------------------------

{-# INLINE unsafeByte #-}
unsafeByte :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
unsafeByte b p = do
  poke p b
  return (p `plusPtr` 1)

data OutCmd
  = OWord64 {-# UNPACK #-} !Word64
  | OInt64  {-# UNPACK #-} !Int64
  -- | OBytesBS  {-# UNPACK #-} !B.ByteString
  -- | OBytesLBS {-# UNPACK #-} !LB.ByteString
  | OFalse
  | OTrue
  | ONull
  -- | OArray OutStream
  -- | OUtf8s String
  -- | OUtf8b !B.ByteString
  -- | OBytes !B.ByteString
  --  OUtf8b B.ByteString
  --  OUtf8a Int# Addr#

data InCmd
  = IWord64 {-# UNPACK #-} !Word64
  | IInt64 {-# UNPACK #-} !Int64
  deriving Show

-- TODO: Use proper Stream (i.e., non-recursive)
type OutStream = [OutCmd]

w2b :: Word64 -> Word8
w2b = fromIntegral

i2b :: Int -> Word8
i2b = fromIntegral

encodeOutStream :: OutStream -> BuildStep ()
encodeOutStream cmds_ (BufferRange lo hi) = do
  putStrLn $ "filling " ++ show lo ++ "-" ++ show hi
  go lo hi cmds_
 where
   go :: Ptr Word8 -> Ptr Word8 -> OutStream -> IO (BuildSignal ())
   go lo hi cmds0@(cmd:cmds) = case cmd of
     OFalse | lo < hi -> do
       lo' <- unsafeByte 0xf4 lo
       go lo' hi cmds
     OTrue | lo < hi -> do
       lo' <- unsafeByte 0xf5 lo
       go lo' hi cmds
     ONull | lo < hi -> do
       lo' <- unsafeByte 0xf6 lo
       go lo' hi cmds
     OWord64 w | (lo `plusPtr` 9) <= hi -> do
       print lo
       lo' <- unsafeWord64 0 w lo
       go lo' hi cmds
     OInt64 i | (lo `plusPtr` 9) <= hi -> do
       let !sign = i `shiftR` 63
           !mt = fromIntegral (sign .&. 0x20) :: Word8
           !w = fromIntegral (sign `xor` i)
       lo' <- unsafeWord64 mt w lo
       go lo' hi cmds
     _ -> do
      print ("buffer full", lo, hi)
      return (BufferFull 9 lo (\ (BufferRange lo' hi') -> go lo' hi' cmds0))
   go lo _hi [] = do
     putStrLn $ "Done " ++ show lo
     return (Done lo ())

     
     -- OBytesBS bs | (lo `plusPtr` 9) <= hi -> do
     --   writeBytes bs

type MajorType = Word8

{-# INLINE unsafeWord64 #-}
unsafeWord64 :: MajorType -> Word64 -> Ptr Word8 -> IO (Ptr Word8)
unsafeWord64 mt w p
  | w < 24  = unsafeByte (mt + w2b w) p
  | w < 256 = do p1 <- unsafeByte (mt + 0x18) p
                 unsafeByte (w2b w) p1
  | w < 65536 = do p1 <- unsafeByte (mt + 0x19) p
                   p2 <- unsafeByte (w2b (w `shiftR` 8)) p1
                   unsafeByte (w2b w) p2
  | w < 4294967295
  = do p1 <- unsafeByte (mt + 0x1a) p
       p2 <- unsafeByte (w2b (w `shiftR` 24)) p1
       p3 <- unsafeByte (w2b (w `shiftR` 16)) p2
       p4 <- unsafeByte (w2b (w `shiftR`  8)) p3
       unsafeByte (w2b w) p4
  | otherwise
  = do p1 <- unsafeByte (mt + 0x1b) p
       p2 <- unsafeByte (w2b (w `shiftR` 56)) p1
       p3 <- unsafeByte (w2b (w `shiftR` 48)) p2
       p4 <- unsafeByte (w2b (w `shiftR` 40)) p3
       p5 <- unsafeByte (w2b (w `shiftR` 32)) p4
       p6 <- unsafeByte (w2b (w `shiftR` 24)) p5
       p7 <- unsafeByte (w2b (w `shiftR` 16)) p6
       p8 <- unsafeByte (w2b (w `shiftR`  8)) p7
       unsafeByte (w2b w) p8

decodeCBOR :: LB.ByteString -> [InCmd]
decodeCBOR lbs_ = goCs (LB.toChunks lbs_)
 where
   goCs [] = []
   goCs (c:cs)
     | B.null c  = goCs cs
     | otherwise = goN (go1C c) cs --(goCs cs)

   goN d cs = case d of
     DDone      -> case cs of
                     [] -> []
                     _ -> goM go1C cs
     DStep i k  -> i : goN (k ()) cs
     DMore k    -> goM k cs
     DError msg -> error $ "Parse error: " ++ show msg

   goM k [] = error "not enough input"
   goM k (c:cs) | B.null c  = goM k cs
                | otherwise = goN (k c) cs

   go1C !chunk_ = go chunk_ 0 (B.length chunk_)

   --go !c !i !imax | trace ("GO" ++ show (B.unpack c, i, imax)) False = undefined
   go !c !i !imax | i >= imax = DDone
   go !c !i !imax =
     let yield n !c' !i' imax' = DStep n (\_ -> go c' i' imax') in
     let !w0 = B.unsafeIndex c i in
     let !mt = w0 `shiftR` 5 in
     case mt of
       0 -> -- Word64 / positive Int64
         case w0 of
           w | w < 24    ->
               yield (IWord64 (b2w w)) c (i + 1) imax
             | w == 0x18 ->
               get1 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IWord64 w') c' i' imax')
             | w == 0x19 ->
               get2 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IWord64 w') c' i' imax')
             | w == 0x1a ->
               get4 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IWord64 w') c' i' imax')
             | w == 0x1b ->
               get8 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IWord64 w') c' i' imax')
             | otherwise ->
                 DError ("Cannot decode Word64 (w=" ++ show w)
       1 -> -- negative Int64
          let neg n = (-(fromIntegral n)) - 1 in
          case w0 - 0x20 of
           w | w < 24    ->
               yield (IInt64 (neg (b2w w))) c (i + 1) imax
             | w == 0x18 ->
               get1 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IInt64 (neg w')) c' i' imax')
             | w == 0x19 ->
               get2 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IInt64 (neg w')) c' i' imax')
             | w == 0x1a ->
               get4 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IInt64 (neg w')) c' i' imax')
             | w == 0x1b ->
               get8 c (i + 1) imax (\ c' i' imax' w' ->
                 yield (IInt64 (neg w')) c' i' imax')
             | otherwise ->
                 DError ("Cannot decode Int64 (w=" ++ show w)
            
       _ -> error $ "TODO: decoding error (NYI: mt=" ++ show (mt, w0)
    
   -- get1 !c !i !imax
   --   | i < imax  = ( B.unsafeIndex c i, i + 1, imax )
   --   | otherwise = (

data GetM a = GetM{ unGetM :: B.ByteString -> Int -> Int
                           -> (B.ByteString -> Int -> Int -> a -> DecodeResult)
                           -> DecodeResult }

runGetM :: B.ByteString -> Int -> Int -> GetM a -> DecodeResult
runGetM c i imax m = unGetM m c i imax (\_c' _i' _imax' _ -> DDone)

instance Monad GetM where
  return x = GetM $ \c i imax k -> k c i imax x
  m >>= g  = GetM $ \c i imax k ->
    unGetM m c i imax (\c' i' imax' a -> unGetM (g a) c' i' imax' k)

data DecodeResult
  = DDone
  | DStep !InCmd !(() -> DecodeResult)
  | DMore !(B.ByteString -> DecodeResult)  -- bytestring must be non-empty
  | DError String

get1 :: B.ByteString -> Int -> Int
     -> (B.ByteString -> Int -> Int -> Word64 -> DecodeResult)
     -> DecodeResult
get1 !c !i !imax k
  -- trace ("get1 " ++ show (B.unpack c, i, imax)) False = undefined
  | i < imax  = k c (i + 1) imax (b2w (B.unsafeIndex c i))
  | otherwise = DMore (\c' -> get1 c' 0 (B.length c') k)

get2 :: B.ByteString -> Int -> Int
     -> (B.ByteString -> Int -> Int -> Word64 -> DecodeResult)
     -> DecodeResult
get2 !c !i !imax k
  | i < imax - 1 =  -- fast path
    --trace ("get2 fast " ++ show (i, imax)) $
    let !w1 = B.unsafeIndex c i
        !w0 = B.unsafeIndex c (i + 1)
    in k c (i + 2) imax $! (b2w w1 `shiftL` 8) .|. b2w w0
  | otherwise =
      --trace ("get2 slow " ++ show (i, imax)) $
      get1 c i imax (\c' i' imax' w1 ->
      get1 c' i' imax' (\c'' i'' imax'' w0 ->
      k c'' i'' imax'' $! (w1 `shiftL` 8) .|. w0))

get4 :: B.ByteString -> Int -> Int
     -> (B.ByteString -> Int -> Int -> Word64 -> DecodeResult)
     -> DecodeResult
get4 !c !i !imax k
  | i < imax - 3 =  -- fast path
    --trace ("get4 fast " ++ show (i, imax)) $
    let !w3 = B.unsafeIndex c i
        !w2 = B.unsafeIndex c (i + 1)
        !w1 = B.unsafeIndex c (i + 2)
        !w0 = B.unsafeIndex c (i + 3)
    in k c (i + 4) imax $!
         (b2w w3 `shiftL` 24) .|. (b2w w2 `shiftL` 16) .|.
         (b2w w1 `shiftL`  8) .|. b2w w0
  | otherwise =
      --trace ("get2 slow " ++ show (i, imax)) $
      get1 c  i  imax  (\c1 i1 imax1 w3 ->
      get1 c1 i1 imax1 (\c2 i2 imax2 w2 ->
      get1 c2 i2 imax2 (\c3 i3 imax3 w1 ->
      get1 c3 i3 imax3 (\c4 i4 imax4 w0 ->
      k c4 i4 imax4 $!
        (w3 `shiftL` 24) .|. (w2 `shiftL` 16) .|.
        (w1 `shiftL`  8) .|. w0))))

get8 :: B.ByteString -> Int -> Int
     -> (B.ByteString -> Int -> Int -> Word64 -> DecodeResult)
     -> DecodeResult
get8 !c !i !imax k
  | i < imax - 7 =  -- fast path
    --trace ("get4 fast " ++ show (i, imax)) $
    let !w7 = B.unsafeIndex c i
        !w6 = B.unsafeIndex c (i + 1)
        !w5 = B.unsafeIndex c (i + 2)
        !w4 = B.unsafeIndex c (i + 3)
        !w3 = B.unsafeIndex c (i + 4)
        !w2 = B.unsafeIndex c (i + 5)
        !w1 = B.unsafeIndex c (i + 6)
        !w0 = B.unsafeIndex c (i + 7)
    in k c (i + 8) imax $!
         (b2w w7 `shiftL` 56) .|. (b2w w6 `shiftL` 48) .|.
         (b2w w5 `shiftL` 40) .|. (b2w w4 `shiftL` 32) .|.
         (b2w w3 `shiftL` 24) .|. (b2w w2 `shiftL` 16) .|.
         (b2w w1 `shiftL`  8) .|. b2w w0
  | otherwise =
    get4 c i imax (\c1 i1 imax1 w1 ->
    get4 c1 i1 imax1 (\c2 i2 imax2 w0 ->
      k c2 i2 imax2 $! (w1 `shiftL` 32) .|. w0))

-- get2 :: B.ByteString -> Int -> Int 
--      -> 

--newtype StepM a = StepM (B.ByteString -> Int -> Int ->

test2 =
  print . decodeCBOR . LB.fromChunks . map B.pack $
    [[0x38, 0x63]]
  
    -- [[12, 24, 40, 25], [20, 23, 26, 1, 2], [3, 4, 27, 1, 0],
    --  [0, 0, 0, 0, 0, 0], [27, 2, 0, 0, 0, 0, 0, 0, 0, 15]]

{-
encodeChunks :: OutStream -> IO [B.ByteString]
encodeChunks cmds0 = do
  stop

  fp <- mallocForeignPtrBytes chunkSize
  withForeignPtr fp $ \p -> do
    (leftover, k) <- go cmds0 chunkSize p
    return c
 where
   go (cmd:cmds) !avail !p =
     case cmd of
       OFalse | avail >= 1 -> do
         poke p 0xf4
         go cmds (avail - 1) (p `plusPtr` 1)
       OWord64 w | avail >= 9 -> do
         n <- pokeWord 0 p avail
         go cmds (avail - n) (p `plusPtr` n)
       otherwise ->
         -- TODO: grab new buffer
   
   -- PRE: at least 9 bytes available
   {-# INLINE pokeWord #-}
   pokeWord mt w p avail
     | w < 24 -> do poke p (mt + w2b w)
                    return 1
     | w < 256 -> do poke p (mt + 0x18)
                     poke (p `plusPtr` 1) (w2b w)
                     return 2
-} 

{-
encodeReference :: OutStream -> [Word8]
encodeReference cmds0 = encodeN [] cmds0 []
 where
   encodeN s (cmd:cmds) k = case cmd of
     OFalse -> 0xf4 : encodeN s cmds k
     OTrue  -> 0xf5 : encodeN s cmds k
     ONull  -> 0xf6 : encodeN s cmds k
     OWord64 w -> encodeWord 0 w (encodeN s cmds k)
     OInt64 i ->
       let !sign = i `shiftR` 63
           !mt = fromIntegral (sign .&. 0x20) :: Word8
           !w = fromIntegral (sign `xor` i)
       in encodeWord mt w (encodeN s cmds k)
     OBytes dat ->
       encodeWord 0x40 (fromIntegral (B.length dat))
         (bytes dat (encodeN s cmds k))
     OArray cmds' ->
       0x9f : encodeN s cmds' (0xff : encodeN s cmds k)
     OUtf8s str ->
       0x7f : utf8 str (0xff : encodeN s cmds k)
     OUtf8b bs ->
       -- We assume bs is already UTF8-encoded
       encodeWord 0x60 (fromIntegral (B.length bs))
         (bytes bs (encodeN s cmds k))

     -- OEndArray ->
     --   | InArray : s' <- s -> 0xff : encode1 s' cmds
     --   | otherwise -> error "EncodingError: Mismatched OEndArray"

   encodeN s [] k = k
     
   -- encodeInArray [] = error "EncodingError: Mismatched OBeginArray/OEndArray"
   -- encodeInArray (c : cmds) = case c of
   --   OEndArray -> 0xff : encode1 [] cmds
     
   encodeWord :: Word8 -> Word64 -> [Word8] -> [Word8]
   encodeWord mt w k
     | w < 24         = (mt + w2b w) : k
     | w < 256        = (mt + 0x18) : w2b w : k
     | w < 65536      = (mt + 0x19) : w2b (w `shiftR` 8) : w2b w : k
     | w <= 4294967295 = 
        (mt + 0x1a) : w2b (w `shiftR` 24) : w2b (w `shiftR` 16) :
        w2b (w `shiftR` 8) : w2b w : k
     | otherwise      = 
        (mt + 0x1b) : w2b (w `shiftR` 56) : w2b (w `shiftR` 48) :
        w2b (w `shiftR` 40) : w2b (w `shiftR` 32) :
        w2b (w `shiftR` 24) : w2b (w `shiftR` 16) : 
        w2b (w `shiftR` 8) : w2b w : k

   bytes :: B.ByteString -> [Word8] -> [Word8]
   bytes bs0 k = go (B.length bs0) 0 bs0 
    where go  0 !i !bs = k
          go !n !i !bs = bs `B.unsafeIndex` i : go (n - 1) (i + 1) bs


   utf8 :: String -> [Word8] -> [Word8]
   utf8 s k = 
     -- TODO: This traverses the bytes twice, we could do *much* better if we
     -- cared about performance.  E.g., emit a length prefix first, then encode
     -- bytes while they fit the buffer and fill in the prefix afterward.
     let chunk = go [] in
     encodeWord 0 (fromIntegral (length chunk)) (chunk ++ k)
    where
      go [] = k
      go (c:cs) = case ord c of
        n | n < 0x80  -> i2b n : go cs
          | n < 0x800 -> i2b (0xc0 .|. (n `shiftR` 6)) : contByte n 0 : go cs
          | n < 0x10000 ->
            i2b (0xe0 .|. (n `shiftR` 12)) :
            contByte n 6 : contByte n 0 : go cs
          | n < 0x200000 -> 
            i2b (0xf0 .|. (n `shiftR` 18)) :
            contByte n 12 : contByte n 6 : contByte n 0 : go cs
          | n < 0x4000000 -> 
            i2b (0xf8 .|. (n `shiftR` 24)) : contByte n 18 : contByte n 12 :
            contByte n 6 : contByte n 0 : go cs
          | n <= 0x7fffffff -> 
            i2b (0xfc .|. (n `shiftR` 30)) : contByte n 24 : contByte n 18 :
            contByte n 12 : contByte n 6 : contByte n 0 : go cs
          | otherwise -> error "Character code too large"

   contByte :: Int -> Int -> Word8
   contByte n s = i2b (0x80 .|. ((n `shiftR` s) .&. 0x3f))
-}

{-
decodeReference :: [Word8] -> OutStream
decodeReference [] = []
decodeReference (b:bs) =
  let !mt = b `shiftR` 5 in
  case mt of
    0 -> 
      -- Positive Integer (0..2^64-1)
      let (!w, !bs') = decodeW64 (b .&. 0x1f) bs in
      OWord64 w : decodeReference bs'
    1 ->
      -- Negative Integer (-1..-2^64)
      let (!w, !bs') = decodeW64 (b .&. 0x1f) bs in
      OInt64 ((-fromIntegral w) - 1) : decodeReference bs'
    2 ->
      let !lo = b .&. 0x1f in
      case b .&. 0x1f of
        l | l <= 0x1b ->
            let (!w, bs') = decodeW64 l bs in
                (bsb, bs'') = takeStrict w [] bs'
            OBytesBS (B.pack bsb) : decodeReference bs''
          | l == 0x1f ->
            let (cs, bs') = decodeChunks bs in
            OBytesLBS (LB.fromChunks cs) : decodeReference bs'
          | otherwise ->
            error "Could not decode bytestring length"
    3 ->
      error "TODO: decode utf8 string"
    4 ->
      error "TODO: decode array"
    5 ->
      error "TODO: decode map"
    6 ->
      error "TODO: decode tags"
    7 ->
      error "TODO: decode simple value"
    
--    1 -> let (i, bs') = decode
 where
   decodeW64 i bs
     | i < 24 = (b2w i, bs)
     | i == 24, b:bs' <- bs = (b2w b, bs')
     | i == 25, b1:b0:bs' <- bs = ((b2w b1 `shiftL` 8) .|. b2w b0, bs')
     | i == 26, b3:b2:b1:b0:bs' <- bs
     = (shiftW b3 24 .|. shiftW b2 16 .|. shiftW b1 8 .|. b2w b0, bs')
     | i == 27, b7:b6:b5:b4:b3:b2:b1:b0:bs' <- bs
     = (shiftW b7 56 .|. shiftW b6 48 .|. shiftW b5 40 .|. shiftW b4 32 .|.
        shiftW b3 24 .|. shiftW b2 16 .|. shiftW b1  8 .|. b2w b0, bs')
     | otherwise = error "Could not decode number"

   shiftW b s = b2w b `shiftL` s

   takeStrict 0 acc bs = (reverse acc, bs)
   takeStrict !n acc (b:bs) = takeStrict (n - 1) (b:acc) bs
   takeStrict n _acc [] = error $ "Bytes expected: " ++ show n
-}

{-# INLINE b2w #-}
b2w :: Word8 -> Word64
b2w = fromIntegral
     
