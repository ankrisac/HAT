module Encode (
  EncodeLE,
  EncodeBE,
  DecodeLE,
  DecodeBE,

  encodeLE,
  encodeBE,
  decodeLE,
  decodeBE,

  Encoder(..),
  Decoder(..)
) where

import qualified Data.Binary          as B
import qualified Data.Binary.Get      as B
import qualified Data.Binary.Put      as B

import qualified Data.ByteString.Lazy as BS

import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.Word            (Word16, Word32, Word64, Word8)


-- | Encode data in Little-endian. Is a wrapper over 'Data.Binary.Put.PutM'
newtype EncodeLE a = EncodeLE (B.PutM a)
  deriving newtype (Functor, Applicative, Monad)

newtype EncodeBE a = EncodeBE (B.PutM a)
  deriving newtype (Functor, Applicative, Monad)

encodeLE :: EncodeLE () -> BS.ByteString
encodeLE (EncodeLE x) = B.runPut x

encodeBE :: EncodeBE () -> BS.ByteString
encodeBE (EncodeBE x) = B.runPut x



class Monad m => Encoder m where
  putBytes :: BS.ByteString -> m ()

  putU8  :: Word8  -> m ()
  putU16 :: Word16 -> m ()
  putU32 :: Word32 -> m ()
  putU64 :: Word64 -> m ()

  putI8  :: Int8  -> m ()
  putI16 :: Int16 -> m ()
  putI32 :: Int32 -> m ()
  putI64 :: Int64 -> m ()

instance Encoder EncodeLE where
  putBytes = EncodeLE . B.putLazyByteString

  putU8  = EncodeLE . B.putWord8
  putU16 = EncodeLE . B.putWord16le
  putU32 = EncodeLE . B.putWord32le
  putU64 = EncodeLE . B.putWord64le

  putI8  = EncodeLE . B.putInt8
  putI16 = EncodeLE . B.putInt16le
  putI32 = EncodeLE . B.putInt32le
  putI64 = EncodeLE . B.putInt64le

instance Encoder EncodeBE where
  putBytes = EncodeBE . B.putLazyByteString

  putU8  = EncodeBE . B.putWord8
  putU16 = EncodeBE . B.putWord16be
  putU32 = EncodeBE . B.putWord32be
  putU64 = EncodeBE . B.putWord64be

  putI8  = EncodeBE . B.putInt8
  putI16 = EncodeBE . B.putInt16be
  putI32 = EncodeBE . B.putInt32be
  putI64 = EncodeBE . B.putInt64be



newtype DecodeLE a = DecodeLE (B.Get a)
  deriving newtype (Functor, Applicative, Monad)

newtype DecodeBE a = DecodeBE (B.Get a)
  deriving newtype (Functor, Applicative, Monad)

decodeLE :: DecodeLE a -> BS.ByteString -> a
decodeLE (DecodeLE decoder) = B.runGet decoder 

decodeBE :: DecodeBE a -> BS.ByteString -> a
decodeBE (DecodeBE decoder) = B.runGet decoder 


class Monad m => Decoder m where
  getBytes :: Int64 -> m BS.ByteString

  getU8  :: m Word8
  getU16 :: m Word16
  getU32 :: m Word32
  getU64 :: m Word64

  getI8  :: m Int8
  getI16 :: m Int16
  getI32 :: m Int32
  getI64 :: m Int64

instance Decoder DecodeLE where
  getBytes = DecodeLE . B.getLazyByteString

  getU8  = DecodeLE B.getWord8
  getU16 = DecodeLE B.getWord16le
  getU32 = DecodeLE B.getWord32le
  getU64 = DecodeLE B.getWord64le

  getI8  = DecodeLE B.getInt8
  getI16 = DecodeLE B.getInt16le
  getI32 = DecodeLE B.getInt32le
  getI64 = DecodeLE B.getInt64le

instance Decoder DecodeBE where
  getBytes = DecodeBE . B.getLazyByteString

  getU8  = DecodeBE B.getWord8
  getU16 = DecodeBE B.getWord16le
  getU32 = DecodeBE B.getWord32le
  getU64 = DecodeBE B.getWord64le

  getI8  = DecodeBE B.getInt8
  getI16 = DecodeBE B.getInt16le
  getI32 = DecodeBE B.getInt32le
  getI64 = DecodeBE B.getInt64le
