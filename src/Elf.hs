{-# LANGUAGE OverloadedStrings #-}

module Elf where

import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.Semigroup          (stimes)
import           Data.Word               (Word16, Word32, Word64, Word8)
import           GHC.Generics            (Generic)

import           Data.Bits               (Bits)
import           Encode

import qualified Data.ByteString.Builder as B

import System.Process



demo :: IO ()
demo = do
  let file = "out/test"

  B.writeFile file $ encodeLE $ put head
  r <- createProcess $ proc "readelf" ["-h", file] 
  pure ()  
  where
    target = Target W64 LE (Abi SysV 0) EExec Amd64
    head = Head target (Flags 0) :: Head W64



class
  ( Put (Uint w), Integral (Uint w), Bits (Uint w)
  , Put (Sint w), Integral (Sint w), Bits (Sint w)
  ) => Elf (w :: Width) where
  type Uint w = a | a -> w
  type Sint w = a | a -> w
  uint :: Encode s => Uint w -> s
  sint :: Encode s => Sint w -> s
  getWidth :: proof w -> Width


instance Elf W32 where
  type Uint W32 = Word32
  type Sint W32 = Int32
  uint = u32
  sint = i32
  getWidth _ = W32

instance Elf W64 where
  type Uint W64 = Word64
  type Sint W64 = Int64
  uint = u64
  sint = i64
  getWidth _ = W64

{-
  According to the System-V ABI, 'Abi' can specify non-standard 
  layouts or sizes for the main/segment/section headers, but we're
  ignoring that here, since only serialization (with no extensions)
  is required. 
-} 
headSize, progSize, sectSize :: Num p => Width -> p
headSize W32 = 0x34
headSize W64 = 0x40

progSize W32 = 0x20
progSize W64 = 0x38

sectSize W32 = 0x28
sectSize W64 = 0x40


data Head w = Head Target Flags

instance Elf w => Put (Head w) where
  put head@(Head target flags) = put target
    <> version
    <> uint (entry :: Uint w)
    <> uint (progOff :: Uint w)
    <> uint (sectOff :: Uint w)
    <> put flags
    <> u16 (headSize width)
    <> u16 (progSize width)
    <> u16 progNum
    <> u16 (sectSize width)
    <> u16 sectNum
    <> u16 stringNum
    where
      version = u32 1

      width = getWidth head
      entry = 0
      progOff = 0
      sectOff = 0

      progNum = 0
      sectNum = 0
      stringNum = 0



-- | Corresponds to (e_ident, e_type, e_machine) in the ELF specification
data Target = Target Width Endian Abi EType Machine

instance Put Target where
  put (Target width endian abi typ machine) = magic
    <> put width
    <> put endian
    <> version
    <> put abi
    <> padding
    <> put typ
    <> put machine
    where
      version = u8 1
      magic = u8 0x7F <> bytes "ELF"
      padding = stimes 7 (u8 0)

data Endian 
  = LE -- 2's compliment little-endian 
  | BE -- 2's compliment big-endian

instance Put Endian where
  put LE = u8 1
  put BE = u8 2

-- | Processor Architecture width
data Width 
  = W32 -- ^ 32-bit machines 
  | W64 -- ^ 64-bit machines

instance Put Width where
  put W32 = u8 1
  put W64 = u8 2

{- | 
  OS/Abi-specific ELF extensions used in the file. 
  0 indicates unspecified version of 'Abi'
-}
data Abi = Abi OS Word8
  deriving (Generic, Put)

{- |
  Standard Extensions (ie. those other than 'SysV') are only here 
  for completeness. They are currently are untested

  Values of 'OSExt' in the range 64-255 are interpreted with
  reference to the value of the 'Machine' field in 'Target' 
-}
data OS
  = SysV     -- ^ No Extensions
  | HP_UX    -- ^ HP Unix
  | NetBSD
  | GNU
  | Solaris  -- ^ Sun/Oracle Solaris
  | AIX
  | IRIX
  | FreeBSD
  | Tru64    -- ^ Compaq Tru64 UNIX
  | Modesto  -- ^ Novello Modesto
  | OpenBSD
  | OpenVMS
  | HP_NSK   -- ^ HP Non-Stop Kernel
  | AROS     -- ^ Amiga Research OS
  | Fenix    -- ^ FenixOS
  | CloudABI -- ^ Nuxi CloudABI
  | OpenVOS  -- ^ Stratus Technologies OpenVOS
  | OSExt Word8

instance Put OS where
  put = u8 . \case
    SysV     -> 0
    HP_UX    -> 1
    NetBSD   -> 2
    GNU      -> 3
    Solaris  -> 6
    AIX      -> 7
    IRIX     -> 8
    FreeBSD  -> 9
    Tru64    -> 10
    Modesto  -> 11
    OpenBSD  -> 12
    OpenVMS  -> 13
    HP_NSK   -> 14
    AROS     -> 15
    Fenix    -> 16
    CloudABI -> 17
    OpenVOS  -> 18
    OSExt os -> os


-- | Elf file type 
data EType
  = ENone -- ^ No file type
  | ERel  -- ^ Relocatable file
  | EExec -- ^ Executable file
  | EDyn  -- ^ Shared object file
  | ECore -- ^ Core file. Contents are unspecified by ELF
  deriving Enum
  deriving Put via U16 EType

data Machine
  = MachineNone -- ^ No processor specified
  | Intel386    -- ^ x86-32
  | Amd64       -- ^ x86-64

instance Put Machine where
  put = u16 . \case
    MachineNone -> 0
    Intel386    -> 3
    Amd64       -> 62



-- | Processor specific flags
newtype Flags = Flags Word32
  deriving newtype Put 