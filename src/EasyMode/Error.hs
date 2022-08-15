{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EasyMode.Error (
    module Export,
    complain,
    assume,
    impossible,
    assert,
    buckpassing,
) where

import Control.Exception.Extra as Export (Partial)
import Data.Bool (Bool)
import Data.List ((++))
import Data.Text (Text, pack, unpack)
import qualified GHC.Err as E (error, errorWithoutStackTrace)
import GHC.Exts (RuntimeRep, TYPE)
import GHC.Stack (SrcLoc (..), callStack, freezeCallStack, getCallStack, popCallStack, prettyCallStack, withFrozenCallStack)
import GHC.Stack as Export (HasCallStack)
import Prelude (fst, head, length, show, snd, tail, (>), (>=))

complain :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => Text -> a
complain msg =
    let ?callStack = freezeCallStack (let n = length (getCallStack callStack) in if n > 2 then popCallStack (popCallStack callStack) else if n > 1 then popCallStack callStack else callStack)
     in let stack = (getCallStack callStack)
            stack0 = head stack
            stack1 = head (tail stack)
            callee_fname = fst stack0
            caller_fname = fst stack1
            loc = snd stack0
            caller_info = if length stack >= 2 then "\n Bug function: `" ++ caller_fname ++ "` (Partial without complain)" else ""
            loc_info =
                loc.srcLocFile ++ ":" ++ show loc.srcLocStartLine ++ ":" ++ show loc.srcLocStartCol
                    ++ " ~ "
                    ++ show loc.srcLocEndLine
                    ++ ":"
                    ++ show loc.srcLocEndCol
         in E.error ("[Improper Call to `" ++ callee_fname ++ "`]: " ++ unpack msg ++ caller_info ++ "\n Bad code at: " ++ loc_info ++ "\n")

assume :: HasCallStack => Bool -> a -> a
assume c r =
    if c
        then r
        else
            let ?callStack = freezeCallStack (let n = length (getCallStack callStack) in if n > 2 then popCallStack (popCallStack callStack) else if n > 1 then popCallStack callStack else callStack)
             in let msg = "assumption failed!"
                    stack = (getCallStack callStack)
                    stack0 = head stack
                    stack1 = head (tail stack)
                    callee_fname = fst stack0
                    caller_fname = fst stack1
                    loc = snd stack0
                    caller_info = if length stack >= 2 then "\n Bug function: `" ++ caller_fname ++ "` (Partial without complain)" else ""
                    loc_info =
                        loc.srcLocFile ++ ":" ++ show loc.srcLocStartLine ++ ":" ++ show loc.srcLocStartCol
                            ++ " ~ "
                            ++ show loc.srcLocEndLine
                            ++ ":"
                            ++ show loc.srcLocEndCol
                 in E.error ("[Improper Call to `" ++ callee_fname ++ "`]: " ++ msg ++ caller_info ++ "\n Bad code at: " ++ loc_info ++ "\n")

impossible :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a
impossible =
    let ?callStack = freezeCallStack (let n = length (getCallStack callStack) in if n > 2 then popCallStack (popCallStack callStack) else if n > 1 then popCallStack callStack else callStack)
     in let stack = (getCallStack callStack)
            stack0 = head stack
            fname = fst stack0
            loc = snd stack0
            loc_info =
                loc.srcLocFile ++ ":" ++ show loc.srcLocStartLine ++ ":" ++ show loc.srcLocStartCol
                    ++ " ~ "
                    ++ show loc.srcLocEndLine
                    ++ ":"
                    ++ show loc.srcLocEndCol
         in E.error ("[Internal Error inside `" ++ fname ++ "`]: impossible branch accessed!" ++ "\n Bad code at: " ++ loc_info ++ "\n")

assert :: Bool -> a -> a
assert c r =
    if c
        then r
        else
            let ?callStack = freezeCallStack (let n = length (getCallStack callStack) in if n > 2 then popCallStack (popCallStack callStack) else if n > 1 then popCallStack callStack else callStack)
             in let stack = (getCallStack callStack)
                    stack0 = head stack
                    fname = fst stack0
                    loc = snd stack0
                    loc_info =
                        loc.srcLocFile ++ ":" ++ show loc.srcLocStartLine ++ ":" ++ show loc.srcLocStartCol
                            ++ " ~ "
                            ++ show loc.srcLocEndLine
                            ++ ":"
                            ++ show loc.srcLocEndCol
                 in E.error ("[Internal Error inside `" ++ fname ++ "`]: assertion failed!" ++ "\n Bad code at: " ++ loc_info ++ "\n")

buckpassing :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => (HasCallStack => a) -> a
buckpassing x =
    -- TODO: add more info about buckpassing stack
    let ?callStack = freezeCallStack (popCallStack callStack)
     in x
