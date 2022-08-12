{-# language UndecidableInstances #-}

module EasyMode.Layer1.Show where

import Data.Text as Export (Text, pack, unpack)
import qualified Prelude as P

class Show a where
    show :: a -> Text

instance P.Show a => Show a where
    show x = pack (P.show x)

