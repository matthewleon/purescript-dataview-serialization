module Data.ArrayBuffer.DataView.Serialization where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST (ST, newSTRef, pureST)
import Data.Array.ST (STArray, emptySTArray, pushSTArray, unsafeFreeze)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (DataView, ByteOffset)
import Data.Char (fromCharCode)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
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

getASCIIChar :: Decoder Char
getASCIIChar = fromCharCode <$> getUint8

type GetArrayState = Tuple Int ByteOffset

getArray :: forall a. Decoder a -> Int -> Decoder (Array a)
getArray d len = Decoder \dv bo -> pureST do
  stArr <- emptySTArray
  stBo <- newSTRef bo
  stN <- newSTRef len
  let decode = runDecoder d dv
  untilE $
    if stN == 0
    then pure true
    else case decode stBo of
      Just (Tuple bo' x) -> do
        _ <- writeSTRef stBo bo'
        pushSTArray stArr x
        modifySTRef stN (_ - 1)
        pure false
      Nothing -> pure true
  if stN == 0
  then do
    bo' <- readSTRef stBo
    arr <- unsafeFreeze stArr
    pure <<< Just $ Tuple bo' arr
  else pure Nothing
{-
  tailRecM (getArray' arr dv) (Tuple len bo) >>= case _ of
    Just bo' -> pure <<< Just <<< Tuple bo' =<< unsafeFreeze arr
    Nothing  -> pure Nothing
  where
  getArray'
    :: forall h
     . STArray h a
    -> DataView
    -> GetArrayState
    -> Eff (st :: ST h) (Step GetArrayState (Maybe ByteOffset))
  getArray' arr dv = go
    where
    go
      :: Tuple Int ByteOffset
      -> Eff (st :: ST h) (Step GetArrayState (Maybe ByteOffset))
    go (Tuple 0 bo')   = pure $ Done $ Just bo'
    go (Tuple n' bo')  = case runDecoder d dv bo' of
      Just (Tuple bo'' x) -> do
        _ <- pushSTArray arr x
        pure <<< Loop $ Tuple (n' - 1) bo''
      Nothing             -> pure $ Done Nothing
-}

getASCIIString :: Int -> Decoder String
getASCIIString = map fromCharArray <<< getArray getASCIIChar

decoder :: forall a. DV.Getter a -> ByteOffset -> Decoder a
decoder g inc = Decoder decoder'
  where decoder' dv bo = Tuple (bo + inc) <$> g dv bo
