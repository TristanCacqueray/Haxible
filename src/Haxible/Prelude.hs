-- | This module re-exports common functions so that the other modules doesn't have to import them.
module Haxible.Prelude
  ( module Witch,
    module Data.Maybe,
    Text,
    LByteString,
    Data.Aeson.Value (..),
    Data.Aeson.Result (..),
    Data.Aeson.fromJSON,
    Data.Bifunctor.first,
    Data.List.nub,
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
import Data.Aeson.Lens
import Data.Bifunctor qualified
import Data.ByteString.Lazy qualified
import Data.Foldable
import Data.Generics.Labels ()
import Data.List qualified
import Data.Maybe
import Data.Text (Text)
import Debug.Trace qualified
import GHC.Generics qualified
import System.Directory
import System.Environment qualified
import System.FilePath
import Witch

type LByteString = Data.ByteString.Lazy.ByteString
