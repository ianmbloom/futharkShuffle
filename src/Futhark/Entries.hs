
module Futhark.Entries where
import qualified Futhark.Raw as Raw
import qualified Futhark.Context as C
import Futhark.Fut (FutT)
import qualified Futhark.Fut as Fut
import qualified Futhark.Wrap as U
import Futhark.Types
import qualified Futhark.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types

shuffler
  :: Monad m 
  => Int64
  -> Int64
  -> Int64
  -> FutT c m (I64_3d c)
shuffler in0 in1 in2
  =  Fut.unsafeLiftFromIO $ \context
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_shuffler context' out0 in0 in1 in2)
  >> U.peekFreeWrapIn context out0
