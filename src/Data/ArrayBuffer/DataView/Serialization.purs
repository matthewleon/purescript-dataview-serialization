module Data.ArrayBuffer.DataView.Serialization where

import Prelude

import Control.Monad.Eff (untilE)
import Control.Monad.ST (modifySTRef, newSTRef, pureST, readSTRef, writeSTRef)
import Data.Array.ST (emptySTArray, pushSTArray, unsafeFreeze)
import Data.ArrayBuffer.Safe.DataView as DV
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, toInt)

newtype Decoder a =
  Decoder (DV.DataView -> DV.ByteOffset -> Maybe (Tuple DV.ByteOffset a))

instance functorDecoder :: Functor Decoder
  where
  map :: forall a b. (a -> b) -> Decoder a -> Decoder b
  map f d = Decoder $ \dv bo -> map f <$> runDecoder d dv bo

instance applyDecoder :: Apply Decoder
  where
  apply :: forall a b. Decoder (a -> b) -> Decoder a -> Decoder b
  apply d1 d2 = Decoder $ \dv bo ->
    runDecoder d1 dv bo >>= \(Tuple bo' f) -> runDecoder (f <$> d2) dv bo'

instance applicativeDecoder :: Applicative Decoder
  where
  pure :: forall a. a -> Decoder a
  pure x = Decoder \_ bo -> Just $ Tuple bo x

instance bindDecoder :: Bind Decoder
  where
  bind :: forall a b. Decoder a -> (a -> Decoder b) -> Decoder b
  bind d f = Decoder $ \dv bo ->
    runDecoder d dv bo >>= \(Tuple bo' r) -> runDecoder (f r) dv bo'

instance monadDecoder :: Monad Decoder

runDecoder ::forall a.
  Decoder a -> DV.DataView -> DV.ByteOffset -> Maybe (Tuple DV.ByteOffset a)
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

getUint8 :: Decoder UInt
getUint8 = decoder DV.getUint8 1

getUint16be :: Decoder UInt
getUint16be = decoder DV.getUint16be 2

getUint16le :: Decoder UInt
getUint16le = decoder DV.getUint16le 2

getUint32be :: Decoder UInt
getUint32be = decoder DV.getUint32be 4

getUint32le :: Decoder UInt
getUint32le = decoder DV.getUint32le 4

getASCIIChar :: Decoder Char
getASCIIChar = fromCharCode <<< toInt <$> getUint8

--getTypedArray

skipBytes :: Int -> Decoder Unit
skipBytes n = Decoder \_ bo -> Just $ Tuple (bo + n) unit

setOffset :: DV.ByteOffset -> Decoder Unit
setOffset = Decoder <<< const <<< const <<< Just <<< flip Tuple unit

type GetArrayState = Tuple Int DV.ByteOffset

getArray :: forall a. Decoder a -> Int -> Decoder (Array a)
getArray d len = Decoder \dv bo -> pureST do
  stArr <- emptySTArray
  stBo <- newSTRef bo
  stN <- newSTRef len
  let decode = runDecoder d dv
  untilE $ readSTRef stN >>= case _ of
    0 -> pure true
    n -> decode <$> readSTRef stBo >>= case _ of
      Just (Tuple bo' x) ->
        writeSTRef stBo bo'
        *> pushSTArray stArr x
        *> modifySTRef stN (_ - 1)
        *> pure false
      Nothing -> pure true
  readSTRef stN >>= case _ of
    0 -> map Just $ Tuple <$> readSTRef stBo <*> unsafeFreeze stArr
    _ -> pure Nothing

getASCIIString :: Int -> Decoder String
getASCIIString = map fromCharArray <<< getArray getASCIIChar

getTypedArray
  :: forall t m
   . TA.IsArrayType (TA.ArrayView t) m
  => Decoder (TA.ArrayView t)
getTypedArray = Decoder \dv bo ->
  let ab   = DV.buffer dv
      dvbo = DV.byteOffset dv
  in do ta <- TA.fromArrayBufferWithOffset ab (dvbo + bo)
        pure $ Tuple (bo + TA.byteLength ta) ta

getTypedArrayWithLength
  :: forall t m
   . TA.IsArrayType (TA.ArrayView t) m
  => TA.ByteLength
  -> Decoder (TA.ArrayView t)
getTypedArrayWithLength l = Decoder \dv bo ->
  let ab   = DV.buffer dv
      dvbo = DV.byteOffset dv
  in do ta <- TA.fromArrayBufferWithOffsetAndLength ab (dvbo + bo) l
        pure $ Tuple (bo + TA.byteLength ta) ta

decoder :: forall a. DV.Getter a -> DV.ByteOffset -> Decoder a
decoder g inc = Decoder \dv bo -> Tuple (bo + inc) <$> g dv bo
