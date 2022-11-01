{-# LANGUAGE TemplateHaskell #-}

-- | <https://developers.strava.com/docs/webhooks>
module Strive.Types.Subscription
  ( Subscription(..)
  , SubscriptionDetail(..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteSummary)

-- | <https://developers.strava.com/docs/webhooks>
data Subscription = Subscription
  { subscription_id :: Integer
  }
  deriving Show

$(deriveFromJSON options ''Subscription)

-- | <https://developers.strava.com/docs/webhooks>
data SubscriptionDetail = SubscriptionDetail
  { subscriptionDetail_id :: Integer
  , subscriptionDetail_applicationId :: Integer
  , subscriptionDetail_callbackUrl :: Text
  , subscriptionDetail_createdAt :: UTCTime
  , subscriptionDetail_updatedAt :: UTCTime
  }
  deriving Show

$(deriveFromJSON options ''SubscriptionDetail)
