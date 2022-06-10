{-# language PolyKinds #-}

module EasyMode.Layer1.Error
(
    module Export,
    blame,
    error,
    assume,
    assert,
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

blame :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => Text -> a
blame msg = E.errorWithoutStackTrace ("[Improper Call]: " ++ unpack msg)

error :: forall (r :: RuntimeRep). forall (a :: TYPE r). Text -> a
error msg = E.errorWithoutStackTrace ("[Internal Error]: " ++ unpack msg)

assume :: HasCallStack => Bool -> a -> a
assume c r = if c then r else E.errorWithoutStackTrace "[Improper Call]: assumption failed"

assert :: Bool -> a -> a
assert c r = if c then r else E.errorWithoutStackTrace "[Internal Error]: assertion failed"
