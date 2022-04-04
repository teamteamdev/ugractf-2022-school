{-# LANGUAGE Strict #-}

module TicTacToe.Types where

import GHC.Generics (Generic)
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

yurtaMagic :: Int
yurtaMagic = 13374242

yurtaVersion :: Int
yurtaVersion = 100

data ErrorCode = UnexpectedPacket
               | IncompatibleVersion
               | NotLicensed
               | IllegalTurn
               deriving (Show, Eq, Generic, Binary)

data GameSide = SideX | SideO
              deriving (Show, Eq, Generic, Binary, Hashable)

oppositeSide :: GameSide -> GameSide
oppositeSide SideX = SideO
oppositeSide SideO = SideX

newtype ByteStringS2 = ByteStringS2 { unS2 :: ByteString }
                     deriving (Show, Eq)

instance Binary ByteStringS2 where
    put bs = do
      let len = BS.length (unS2 bs)
      if len > fromIntegral (maxBound :: Word16) then
        error "ByteString is too long"
      else do
        put (fromIntegral len :: Word16)
        putByteString (unS2 bs)
    get = do
      (len :: Word16) <- get
      fmap ByteStringS2 $ getByteString $ fromIntegral len

data Packet = GreetPacket      { greetMagic :: Int, greetVersion :: Int }
            | LicensePacket    { licenseKey :: ByteStringS2 }
            | OKPacket         { }
            | DisconnectPacket { }
            | ErrorPacket      { errorCode :: ErrorCode }
            | NewGamePacket    { newSide :: GameSide }
            | TurnPacket       { turnX :: Word8, turnY :: Word8 }
            | EndGamePacket    { winner :: Maybe GameSide }
            | EchoPacket       { message :: ByteStringS2 }
            deriving (Show, Eq, Generic, Binary)
