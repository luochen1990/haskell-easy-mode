{-# LANGUAGE UndecidableInstances #-}

module EasyMode.Show (
    module Export,
    Show,
    show,
) where

import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import Data.Text.IO as Export (hPutStr, hPutStrLn, putStr, putStrLn, getLine, readFile, writeFile)
import qualified GHC.Real as P (Ratio ((:%)), (%))
import Prelude (Show)
import qualified Prelude as P

import System.IO as IO

-- import qualified TextShow as T
-- import Data.Text.Lazy as T (toStrict)
-- import TextShow (TextShow)

show :: P.Show a => a -> Text
show x = pack (P.show x)

instance {-# OVERLAPPING #-} P.Show P.Rational where
    {-# SPECIALIZE instance P.Show P.Rational #-}
    showsPrec p (x P.:% y) =
        P.showParen (p P.> 7) P.$
            P.showsPrec 8 x
                P.. P.showString " / "
                P.. P.showsPrec 8 y
