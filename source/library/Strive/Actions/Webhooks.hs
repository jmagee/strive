-- | https://developers.strava.com/docs/webhooks/
module Strive.Actions.Webhooks
  ( createSubscription
  , viewSubscription
  ) where

import Network.HTTP.Types (toQuery)
import Strive.Aliases (ApplicationId, ApplicationSecret, RedirectUri, Result)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (buildRequest, get, performRequest, post, put)

-- | Create a webhook subscription
createSubscription
  :: ApplicationId
  -> ApplicationSecret
  -> RedirectUri
  -> String
  -> IO (Result ())
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
  -> IO (Result ())
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
