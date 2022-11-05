{-# LANGUAGE TemplateHaskell #-}

-- | <https://developers.strava.com/docs/webhooks>
module Strive.Types.Subscription
  ( Subscription(..)
  , SubscriptionDetail(..)
  , SubscriptionEvent(..)
  , SubscriptionUpdate(..)
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

-- | <https://developers.strava.com/docs/webhooks>
data SubscriptionUpdate = SubscriptionUpdate
  { subscriptionUpdate_title :: Maybe Text
  , subscriptionUpdate_type  :: Maybe Text
  , subscriptionUpdate_private :: Maybe Text
  , subscriptionUpdate_authorized :: Maybe Text
  }
  deriving Show

$(deriveFromJSON options ''SubscriptionUpdate)

-- | <https://developers.strava.com/docs/webhooks>
data SubscriptionEvent = SubscriptionEvent
  { subscriptionEvent_objectType :: Text
  , subscriptionEvent_objectId :: Integer
  , subscriptionEvent_aspectType :: Text
  , subscriptionEvent_updates :: Maybe SubscriptionUpdate
  , subscriptionEvent_ownerId :: Integer
  , subscriptionEvent_subscriptionId :: Integer
  , subscriptionEvent_eventTime :: Integer
  }
  deriving Show

$(deriveFromJSON options ''SubscriptionEvent)
