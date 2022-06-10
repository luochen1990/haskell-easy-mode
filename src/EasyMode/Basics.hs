module EasyMode.Basics
(
    module Export
)
where

import Data.String as Export (String)
import Data.ByteString as Export (ByteString)
import Data.Char as Export (Char)
import Data.Bool as Export (Bool(..), (&&), (||))
import Data.Ord as Export ((<=), (>=), (<), (>))
import Data.Maybe as Export (fromJust)
import Data.Monoid as Export ((<>))
import GHC.Integer as Export (Integer)
import GHC.Types as Export (Int)
import GHC.Num as Export (Num, (+))
import GHC.Enum as Export (maxBound, minBound)
import GHC.Real as Export (fromIntegral)
--import Data.Either.Extra
import Data.List.Extra as Export (chunksOf)
import GHC.Err as Export (error)
import Control.Exception.Extra as Export (Partial)
import Data.List as Export (iterate)
import Data.Foldable as Export (Foldable)
import Data.Map as Export (Map)
import Data.Set as Export (Set)
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function as Export ((&), id, (.))
import Data.List as Export (iterate, unfoldr)
import Data.Text.Encoding as Export (encodeUtf8, decodeUtf8, decodeLatin1)
import Data.Text as Export (Text, pack, unpack)

--import Data.Time
--import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
--import qualified Turtle
--import Data.ByteArray (ByteArrayAccess)
--import Data.ByteArray.Encoding (convertFromBase, convertToBase, Base (Base16))
--import Crypto.Hash (hashWith, SHA256(..), Digest)
--import Crypto.MAC.HMAC
