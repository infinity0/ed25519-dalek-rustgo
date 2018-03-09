module FLib where

import Data.Word
import Data.ByteString as B

import System.IO.Unsafe

import Foreign.Ptr
import Foreign.Marshal.Array

-- all args are inputs created in haskell and borrowed by rust, and we have no
-- return value; therefore we don't need any "free" tricks as in here:
-- https://github.com/aisamanra/rust-haskell-ffi/blob/master/point.rs
--
-- "unsafe" keyword improves speed, explained here:
-- https://wiki.haskell.org/GHC/Using_the_FFI#Improving_efficiency
foreign import ccall unsafe "scalar_base_mult"
  rawScalarBaseMult :: Ptr Word8 -> Ptr Word8 -> IO ()

unsafeScalarBaseMult :: ByteString -> ByteString
unsafeScalarBaseMult input = unsafePerformIO $ do
    iarray <- newArray $ unpack input
    oarray <- mallocArray 32
    rawScalarBaseMult oarray iarray
    pack <$> peekArray 32 oarray

scalarBaseMult :: ByteString -> Maybe ByteString
scalarBaseMult input = case B.length input of
    32 -> Just $ unsafeScalarBaseMult input
    _ -> Nothing
