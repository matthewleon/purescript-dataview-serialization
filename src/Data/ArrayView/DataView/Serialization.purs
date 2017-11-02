module Data.ArrayView.DataView.Serialization where

import Prelude

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView, ByteOffset)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype Decoder a =
  Decoder (DataView -> ByteOffset -> Maybe (Tuple ByteOffset a))

instance functorDecoder :: Functor Decoder
  where
  map :: forall a b. (a -> b) -> Decoder a -> Decoder b
  map f d = Decoder $ \dv bo -> map f <$> runDecoder d dv bo

instance applyDecoder :: Apply Decoder
  where
  apply :: forall a b. Decoder (a -> b) -> Decoder a -> Decoder b
  apply d1 d2 = Decoder $ \dv bo ->
    case runDecoder d1 dv bo of
      Just (Tuple bo' f) -> runDecoder (f <$> d2) dv bo'
      Nothing -> Nothing

instance applicativeDecoder :: Applicative Decoder
  where
  pure :: forall a. a -> Decoder a
  pure = Decoder <<< const <<< const <<< Just <<< Tuple 0

runDecoder ::forall a.
  Decoder a -> DataView -> ByteOffset -> Maybe (Tuple ByteOffset a)
runDecoder (Decoder d) = d

getInt8 :: Decoder Int
getInt8 = decoder DV.getInt8 1

getInt16be :: Decoder Int
getInt16be = decoder DV.getInt16be 2

getInt16le :: Decoder Int
getInt16le = decoder DV.getInt16le 2

getInt32be :: Decoder Int
getInt32be = decoder DV.getInt32be 4

getInt32le :: Decoder Int
getInt32le = decoder DV.getInt32le 4

getUint8 :: Decoder Int
getUint8 = decoder DV.getUint8 1

getUint16be :: Decoder Int
getUint16be = decoder DV.getUint16be 2

getUint16le :: Decoder Int
getUint16le = decoder DV.getUint16le 2

getUint32be :: Decoder Int
getUint32be = decoder DV.getUint32be 4

getUint32le :: Decoder Int
getUint32le = decoder DV.getUint32le 4

decoder :: forall a. DV.Getter a -> ByteOffset -> Decoder a
decoder g inc = Decoder decoder'
  where decoder' dv bo = Tuple (bo + inc) <$> g dv bo
