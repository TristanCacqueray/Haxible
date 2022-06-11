-- | This module re-exports common functions so that the other modules doesn't have to import them.
module Haxible.Prelude
  ( module Witch,
    module Data.Maybe,
    json,
    Vars,
    mkObj,
    mkKeyMap,
    quote,
    Text,
    LByteString,
    Data.Map.Map,
    Data.Aeson.Value (..),
    Data.Aeson.Result (..),
    Data.Aeson.fromJSON,
    Data.Bifunctor.first,
    Data.List.nub,
    Data.List.sort,
    Data.List.sortOn,
    Data.List.partition,
    GHC.Generics.Generic,
    module Control.Monad.Reader,
    module Control.Monad.State,
    (&),
    (.~),
    (%~),
    Control.Lens.preview,
    Control.Lens.itoListOf,
    Debug.Trace.trace,
    System.Environment.lookupEnv,
    Data.List.NonEmpty.nonEmpty,
    Data.List.NonEmpty.NonEmpty,
    module Data.Aeson.Lens,
    module System.FilePath,
    module Control.Monad,
    module Control.Applicative,
    module Data.Foldable,
    module System.Directory,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Aeson.Lens
import Data.Aeson.QQ
import Data.Bifunctor qualified
import Data.ByteString.Lazy qualified
import Data.Foldable
import Data.Generics.Labels ()
import Data.List qualified
import Data.List.NonEmpty qualified
import Data.Map qualified
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace qualified
import GHC.Generics qualified
import Language.Haskell.TH.Quote qualified
import System.Directory
import System.Environment qualified
import System.FilePath
import Witch

type LByteString = Data.ByteString.Lazy.ByteString

type Vars = [(Text, Data.Aeson.Value)]

-- | Create a key map with left priority
--
-- >>> mkKeyMap [("a", [aesonQQ|true|]), ("a", [aesonQQ|false|])]
-- fromList [("a",Bool True)]
mkKeyMap :: Vars -> Data.Aeson.KeyMap.KeyMap Data.Aeson.Value
mkKeyMap = Data.Aeson.KeyMap.fromList . map (Data.Bifunctor.first Data.Aeson.Key.fromText) . reverse

mkObj :: Vars -> Data.Aeson.Value
mkObj = Data.Aeson.Object . mkKeyMap

quote :: Text -> Text
quote = Text.cons '"' . flip Text.snoc '"'

json :: Language.Haskell.TH.Quote.QuasiQuoter
json = aesonQQ
