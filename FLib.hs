module FLib where

import Data.Word
import Data.ByteString as B
import Data.ByteString.Unsafe

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
unsafeScalarBaseMult input = unsafePerformIO $ --
    unsafeUseAsCStringLen input $ \(iptr, _) -> do
    optr <- mallocArray 32
    let ostr = (optr, 32)
    rawScalarBaseMult (castPtr optr) (castPtr iptr)
    unsafePackMallocCStringLen ostr

scalarBaseMult :: ByteString -> Maybe ByteString
scalarBaseMult input = case B.length input of
    32 -> Just $ unsafeScalarBaseMult input
    _ -> Nothing
