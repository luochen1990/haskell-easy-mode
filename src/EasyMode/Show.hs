{-# language UndecidableInstances #-}

module EasyMode.Show (
    Show(..)
) where

import Data.Text (Text, pack, unpack)
import qualified Prelude as P
import qualified GHC.Real as P ((%), Ratio((:%)))
import qualified TextShow as T
import Data.Function ((&))
import Data.Text.Lazy as T (toStrict)
import TextShow (TextShow)

class Show a where
    show :: a -> Text
    show x = showbPrec 0 x & T.toLazyText & T.toStrict

    showbPrec :: P.Integer -> a -> T.Builder
    showbPrec _ x = T.fromText (show x)


-- {-# OVERLAPS / OVERLAPPING / OVERLAPPABLE #-}

instance {-# OVERLAPPABLE #-} P.Show a => Show a where
    showbPrec p = T.showsToShowb (P.showsPrec (P.fromIntegral p))

-- instance {-# OVERLAPS #-} Show a => P.Show a where
--     show x = unpack (show x)

-- instance {-# OVERLAPPING #-} TextShow a => Show a where
--     show x = showt x

instance {-# OVERLAPPING #-} (P.Show a)  => Show (P.Ratio a)  where
    {-# SPECIALIZE instance Show P.Rational #-}
    showbPrec p = T.showsToShowb (showsPrec (P.fromIntegral p)) where
        showsPrec p (x P.:% y)  =  P.showParen (p P.> 7) P.$
                                   P.showsPrec 8 x P..
                                   P.showString " / " P..
                                   P.showsPrec 8 y

