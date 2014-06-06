{-# LANGUAGE DeriveGeneric #-}
import Development.Scion.Message
import Development.Scion.Binary
import Data.Binary
import GHC.Generics
import System.IO

data Foo
  = X Int Double String
  | Y
  | Z Char Integer
  deriving (Eq, Show, Generic)

instance Binary Foo where put = genput; get = genget

main = do
  let inp = [X 3 5 "foo", Y, Z 'c' 32092093020392]
  withFile "tmp1.bin" WriteMode $ \h -> do
    sendMessage h inp
  withFile "tmp1.bin" ReadMode $ \h -> do
    m <- recvMessage h
    print (inp, m, inp == m)
