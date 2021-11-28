{-# LANGUAGE RankNTypes, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Futhark.Types where
import qualified Futhark.Raw as Raw
import Futhark.Wrap
import Futhark.TypeClasses
import qualified Foreign as F
import qualified Data.Massiv.Array as M
import qualified Control.Concurrent.MVar as MV
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))
import Foreign.Ptr (Ptr)
import Control.DeepSeq (rwhnf)

data I64_3d c = I64_3d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_i64_3d)
instance FutharkObject I64_3d Raw.Futhark_i64_3d where
  wrapFO = I64_3d
  freeFO = Raw.free_i64_3d
  fromFO (I64_3d rc fp) = (rc, fp)
instance FutharkArray I64_3d Raw.Futhark_i64_3d M.Ix3 Int64 where
  shapeFA  = to3d Raw.shape_i64_3d
  newFA    = from3d Raw.new_i64_3d
  valuesFA = Raw.values_i64_3d
