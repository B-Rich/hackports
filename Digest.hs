{-# OPTIONS -#include "openssl/md5.h" #-}
{-# OPTIONS -#include "openssl/sha.h" #-}
{-# OPTIONS -#include "openssl/ripemd.h" #-}

--------------------------------------------------------------------
-- |
-- Module    :  Data.Digest.OpenSSL.MD5
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: Requires FFI
--
--------------------------------------------------------------------
--
-- ByteString-based, zero-copying binding to OpenSSL's md5 interface
--

module Digest where

--
-- A few imports, should tidy these up one day.
--
import qualified Data.ByteString.Char8 as B
import Foreign
import Foreign.C.Types
import Numeric                        (showHex)

md5_digest_length :: Int
md5_digest_length = 16

sha1_digest_length :: Int
sha1_digest_length = 20

ripemd160_digest_length :: Int
ripemd160_digest_length = 20

--
-- | Fast md5 using OpenSSL. The md5 hash should be referentially transparent..
-- The ByteString is guaranteed not to be copied.
--
-- The result string should be identical to the output of MD5(1).
-- That is:
--
-- > $ md5 /usr/share/dict/words 
-- > MD5 (/usr/share/dict/words) = e5c152147e93b81424c13772330e74b3
--
-- While this md5sum binding will return:
--
md5sum :: B.ByteString -> String
md5sum p = unsafePerformIO $ B.useAsCStringLen p $ \(ptr,n) -> do
    allocaBytes md5_digest_length $ \digest_ptr -> do
        digest  <- c_md5 ptr (fromIntegral n) digest_ptr
        go digest 0 []
  where

    -- print it in 0-padded hex format
    go :: Ptr Word8 -> Int -> [String] -> IO String
#ifndef __HADDOCK__
    go q n acc
        | n `seq` q `seq` False = undefined
        | n >= 16   = return $ concat (reverse acc)
        | otherwise = do w <- peekElemOff q n
                         go q (n+1) (draw w : acc)

    draw w = case showHex w [] of
                [x] -> ['0', x]
                x   -> x
#endif

sha1sum :: B.ByteString -> String
sha1sum p = unsafePerformIO $ B.useAsCStringLen p $ \(ptr,n) -> do
    allocaBytes sha1_digest_length $ \digest_ptr -> do
        digest  <- c_sha1 ptr (fromIntegral n) digest_ptr
        go digest 0 []
  where

    -- print it in 0-padded hex format
    go :: Ptr Word8 -> Int -> [String] -> IO String
#ifndef __HADDOCK__
    go q n acc
        | n `seq` q `seq` False = undefined
        | n >= 16   = return $ concat (reverse acc)
        | otherwise = do w <- peekElemOff q n
                         go q (n+1) (draw w : acc)

    draw w = case showHex w [] of
                [x] -> ['0', x]
                x   -> x
#endif

ripemd160sum :: B.ByteString -> String
ripemd160sum p = unsafePerformIO $ B.useAsCStringLen p $ \(ptr,n) -> do
    allocaBytes ripemd160_digest_length $ \digest_ptr -> do
        digest  <- c_ripemd160 ptr (fromIntegral n) digest_ptr
        go digest 0 []
  where

    -- print it in 0-padded hex format
    go :: Ptr Word8 -> Int -> [String] -> IO String
#ifndef __HADDOCK__
    go q n acc
        | n `seq` q `seq` False = undefined
        | n >= 16   = return $ concat (reverse acc)
        | otherwise = do w <- peekElemOff q n
                         go q (n+1) (draw w : acc)

    draw w = case showHex w [] of
                [x] -> ['0', x]
                x   -> x
#endif

--
-- unsigned char *MD5(const unsigned char *d, unsigned long n, unsigned char *md);
--
foreign import ccall "openssl/md5.h MD5" c_md5
    :: Ptr CChar -> CULong -> Ptr CChar -> IO (Ptr Word8)

--
-- unsigned char *SHA1(const unsigned char *d, unsigned long n, unsigned char *md);
--
foreign import ccall "openssl/sha.h SHA1" c_sha1
    :: Ptr CChar -> CULong -> Ptr CChar -> IO (Ptr Word8)

--
-- unsigned char *RIPEMD160(const unsigned char *d, unsigned long n, unsigned char *md);
--
foreign import ccall "openssl/ripemd.h RIPEMD160" c_ripemd160
    :: Ptr CChar -> CULong -> Ptr CChar -> IO (Ptr Word8)
