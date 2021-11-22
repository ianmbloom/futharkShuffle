{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
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
import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import Show
import Data.Maybe
import Data.Int
import Control.Monad
import Foreign.C.Types

type Futhark3d t = Array S Ix3 t

boolToCBool :: Bool -> CBool
boolToCBool True  = CBool 1
boolToCBool False = CBool 0

shuffleGrid :: (Int, Int)
            -> IO ()
shuffleGrid (h, w) =
  do  putStrLn "start"
      ( output   :: Futhark3d Int64) <-
          runFutT $
              do futOutput <- E.shuffler 345 (fromIntegral h) (fromIntegral w)
                 fromFuthark futOutput
      putStrLn "after futhark"
      putStrLn $ "out:\n" ++ showGrid3d output
      putStrLn "done"

sizes :: [(Int,Int)]
sizes = [  (  10,  10)
         , (  10,  10)
        ]

main :: IO ()
main = do
    mapM_ shuffleGrid sizes
