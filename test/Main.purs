module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.ArrayBuffer.DataView.Serialization (Decoder, getASCIIString, getArray, getInt8, getTypedArray, runDecoder)
import Data.ArrayBuffer.Safe.DataView as DV
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.Tuple (Tuple(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let arr = [1, 2, 3, 4]
      dvArr = DV.fromArrayBuffer <<< TA.buffer $ (TA.fromArray arr :: TA.Int8Array)
  logShow arr
  logShow $ runDecoder getInt8 dvArr 0
  logShow $ runDecoder getInt8 dvArr 1
  logShow $ runDecoder getInt8 dvArr 2
  logShow $ runDecoder getInt8 dvArr 3
  logShow $ runDecoder ((_ * 7) <$> getInt8) dvArr 0
  logShow $ runDecoder (getInt8 *> getInt8 *> getInt8) dvArr 0
  logShow $ runDecoder (Tuple <$> getInt8 <*> getInt8) dvArr 0
  logShow $ runDecoder monadicDecoder dvArr 0
  logShow $ runDecoder (getArray getInt8 2) dvArr 1
  logShow $ runDecoder (getASCIIString 2) dvArr 1

  log ""
  log "getTypedArray"
  logShow $ DV.byteOffset dvArr
  logShow $ map TA.show <$> runDecoder (getTypedArray :: Decoder TA.Int8Array) dvArr 0
  logShow $ map TA.show <$> runDecoder (getTypedArray :: Decoder TA.Int16Array) dvArr 0
  logShow $ map TA.show <$> runDecoder (getTypedArray :: Decoder TA.Int16Array) dvArr 1
  logShow $ map TA.show <$> runDecoder (getTypedArray :: Decoder TA.Int16Array) dvArr 2
  
  where
    monadicDecoder = do
      a <- getInt8
      b <- getInt8
      c <- getInt8
      pure [a, b, c]
