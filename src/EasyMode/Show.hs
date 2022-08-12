{-# language UndecidableInstances #-}

module EasyMode.Show where

import Data.Text (Text, pack, unpack)
import qualified Prelude as P

class Show a where
    show :: a -> Text

-- {-# OVERLAPS / OVERLAPPING / OVERLAPPABLE #-}

instance {-# OVERLAPS #-} P.Show a => Show a where
    show x = pack (P.show x)

instance {-# OVERLAPS #-} Show a => P.Show a where
    show x = unpack (show x)

