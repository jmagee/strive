{-# LANGUAGE TemplateHaskell #-}

-- | <https://developers.strava.com/docs/webhooks>
module Strive.Types.Subscription
  ( Subscription(..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteSummary)

-- | <https://developers.strava.com/docs/webhooks>
data Subscription = Subscription
  { subscription_id :: Integer
  , subscription_applicationId :: Integer
  , subscription_callbackUrl :: Text
  , subscription_createdAt :: UTCTime
  , subscription_updatedAt :: UTCTime
  }
  deriving Show

$(deriveFromJSON options ''Subscription)
