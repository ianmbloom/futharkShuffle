{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RankNTypes          #-}
module Main where

import qualified Data.Massiv.Array as A
import Data.Massiv.Array ( Array
                         , S
                         , B
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         )
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import Show
import Data.Maybe
import Data.Int
import Control.Monad
import Foreign.C.Types

type Futhark3d t = Array S Ix3 t

data CountState =
     CountState
     { _counter :: Int
     }
makeLenses ''CountState

type MyMonad c m = StateT CountState (FutT c m)

runMyMonad :: (forall c . MyMonad c IO a) -> IO a
runMyMonad f = do
  (a,s) <- runFutTWith [Debug 1{-, Size "shuffler.group_size_10059" 128-}] $ runStateT f (CountState 0)
  putStrLn $ show (s ^. counter)
  return a

message :: String -> MyMonad c IO ()
message string = lift . lift $ putStrLn string

shuffleGrid :: (Int, Int)
            -> MyMonad c IO ()
shuffleGrid (h, w) =
  do  message "start"
      ( output   :: Futhark3d Int64) <- lift $
              do futOutput <- E.shuffler 345 (fromIntegral h) (fromIntegral w)
                 fromFuthark futOutput
      counter += 1
      message "after futhark"
      message $ "out:\n" ++ showGrid3d output
      message "done"

sizes :: [(Int,Int)]
sizes = [  (  10,  10)
         , (  10,  10)
        ]

main :: IO ()
main = do
    runMyMonad $ mapM_ shuffleGrid sizes
