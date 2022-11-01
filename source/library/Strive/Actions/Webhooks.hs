-- | https://developers.strava.com/docs/webhooks/
module Strive.Actions.Webhooks
  ( createSubscription
  , deleteSubscription
  , viewSubscription
  ) where

import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types (methodDelete, toQuery, noContent204)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Strive.Aliases (ApplicationId, ApplicationSecret, RedirectUri, Result, SubscriptionId)
import Strive.Client (Client, buildClient)
import Strive.Types (Subscription, SubscriptionDetail)
import Strive.Internal.HTTP (buildRequest, get, performRequest, post, put, delete)

-- | Create a webhook subscription
createSubscription
  :: ApplicationId
  -> ApplicationSecret
  -> RedirectUri
  -> String
  -> IO (Result Subscription)
createSubscription clientId secret redirect verify = do
  client <- buildClient Nothing
  post client resource query
 where
  resource = "api/v3/push_subscriptions"
  query =
    toQuery
      [ ("client_id", show clientId)
      , ("client_secret", secret)
      , ("callback_url", redirect)
      , ("verify_token", verify)
      ]

-- | Fetch any webhook subscriptions
viewSubscription
  :: ApplicationId
  -> ApplicationSecret
  -> IO (Result [SubscriptionDetail])
viewSubscription clientId secret = do
  client <- buildClient Nothing
  get client resource query
 where
  resource = "api/v3/push_subscriptions"
  query =
    toQuery
      [ ("client_id", show clientId)
      , ("client_secret", secret)
      ]

-- | Delete any webhook subscription
deleteSubscription
  :: ApplicationId
  -> ApplicationSecret
  -> SubscriptionId
  -> IO (Result ())
deleteSubscription clientId secret sub = do
  client <- buildClient Nothing
  --response <- delete client resource query
  request <- buildRequest methodDelete client resource query
  response <- performRequest client request
  return
    (if responseStatus response == noContent204
      then Right ()
      else Left (response, unpack (toStrict (responseBody response)))
    )
 where
  resource = "api/v3/push_subscriptions/" <> show sub
  query =
    toQuery
      [ ("client_id", show clientId)
      , ("client_secret", secret)
      ]
