{-# language PolyKinds #-}

module EasyMode.Layer1.Error
(
    module Export,
    complain,
    assume,
    impossible,
    assert,
    buckpassing,
)
where

import GHC.Stack as Export (HasCallStack)
import Control.Exception.Extra as Export (Partial)
import Data.Text (Text, pack, unpack)
import qualified GHC.Err as E (error, errorWithoutStackTrace)
import GHC.Exts (RuntimeRep, TYPE)
import Data.Function ((.))
import Data.Bool (Bool)
import Data.List ((++))

complain :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => Text -> a
complain msg = E.error ("[Improper Call]: " ++ unpack msg)

assume :: HasCallStack => Bool -> a -> a
assume c r = if c then r else E.error "[Improper Call]: assumption failed"

impossible :: forall (r :: RuntimeRep). forall (a :: TYPE r). a
impossible = E.errorWithoutStackTrace ("[Internal Error]: impossible branch accessed!")

assert :: Bool -> a -> a
assert c r = if c then r else E.errorWithoutStackTrace "[Internal Error]: assertion failed"

buckpassing :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a -> a
buckpassing x = x

