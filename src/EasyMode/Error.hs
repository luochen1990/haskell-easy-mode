{-# language PolyKinds #-}

module EasyMode.Error
(
    blame,
    error,
    assume,
    assert,
)
where

import Data.Text (Text, pack, unpack)
import qualified GHC.Err as E (error, errorWithoutStackTrace)
import GHC.Stack (HasCallStack)
import GHC.Exts (RuntimeRep, TYPE)
import Data.Function ((.))
import Data.Bool (Bool)
import Data.List ((++))

blame :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => Text -> a
blame msg = E.errorWithoutStackTrace ("Improper Call: " ++ unpack msg)

error :: forall (r :: RuntimeRep). forall (a :: TYPE r). Text -> a
error msg = E.errorWithoutStackTrace ("Internal Error: " ++ unpack msg)

assume :: HasCallStack => Bool -> a -> a
assume c r = if c then r else E.errorWithoutStackTrace "Improper Call: assumption failed"

assert :: Bool -> a -> a
assert c r = if c then r else E.errorWithoutStackTrace "Internal Error: assertion failed"
