module EasyMode.Shell (
    module EasyMode.Shell,
) where

import Data.ByteString (hGet)
import Data.Function (($))
import Data.Text.IO (hGetContents)
import EasyMode.Layers.L1
import GHC.IO.Exception (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode, shell, waitForProcess, withCreateProcess)

shellEval :: Partial => Text -> IO (Either (Int64, Text) Text)
shellEval cmd = do
    (code, out, err) <- readCreateProcessWithExitCode (shell (unpack cmd)) ""
    case code of
        ExitSuccess -> return (Right (pack out))
        ExitFailure errCode -> return (Left (errCode, (pack err)))
