-- | https://developers.strava.com/docs/webhooks/
module Strive.Actions.Webhooks
  ( createSubscription
  ) where

import Network.HTTP.Types (toQuery)
import Strive.Aliases (ApplicationSecret, RedirectUri, Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (buildRequest, get, performRequest, post, put)

createSubscription
  :: Client
  -> ApplicationSecret
  -> RedirectUri
  -> String
  -> IO (Result ())
createSubscription client secret redirect verify = post
  client
  resource
  query
 where
  resource = "api/v3/push_subscriptions"
  query =
    toQuery
      [ ("client_secret", secret)
      , ("redirect_uri", redirect)
      , ("verify_token", verify)
      ]
