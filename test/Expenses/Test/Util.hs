{-# OPTIONS_GHC -Wno-orphans #-}

module Expenses.Test.Util where

import CustomPrelude
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.Text qualified as T
import Database (Contains (..), DoesNotContain (..), IsGTE (..), IsLT (..))
import Expenses.NonEmptyText (NonEmptyText)
import Expenses.NonEmptyText qualified as NET
import Servant (ServerError)
import Types (TagName (..))

instance ToJSON ServerError where
  toJSON = J.toJSON . show @Text

instance IsString NonEmptyText where
  fromString = NET.unsafeFromText . T.pack

deriving newtype instance IsString Contains
deriving newtype instance IsString DoesNotContain
deriving newtype instance Num IsGTE
deriving newtype instance Num IsLT
deriving newtype instance IsString TagName
