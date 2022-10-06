-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
  ( buildAuthorizeUrl
  , exchangeToken
  , refreshExchangeToken
  , deauthorize
  ) where

import Data.ByteString.Char8 (unpack)
import Network.HTTP.Types (Query, renderQuery, toQuery)
import Strive.Aliases
  (ApplicationId, ApplicationSecret, AuthorizationCode, RedirectUri, RefreshToken, Result)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (post)
import Strive.Options (BuildAuthorizeUrlOptions)
import Strive.Types (DeauthorizationResponse, RefreshTokenResponse, TokenExchangeResponse)

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl
  :: ApplicationId -> RedirectUri -> BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUri options =
  "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
 where
  query =
    toQuery
        [ ("client_id", show clientId)
        , ("redirect_uri", redirectUri)
        , ("response_type", "code")
        ]
      <> toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken
  :: ApplicationId
  -> ApplicationSecret
  -> RefreshToken
  -> IO (Result TokenExchangeResponse)
exchangeToken clientId clientSecret code = do
  client <- buildClient Nothing
  post client resource query
 where
  resource = "oauth/token"
  query =
    [ ("client_id", show clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    ]

refreshExchangeToken
  :: ApplicationId
  -> ApplicationSecret
  -> AuthorizationCode
  -> IO (Result RefreshTokenResponse)
refreshExchangeToken clientId clientSecret refresh = do
  client <- buildClient Nothing
  post client resource query
 where
  resource = "oauth/token"
  query =
    [ ("client_id", show clientId)
    , ("client_secret", clientSecret)
    , ("refresh_token", refresh)
    , ("grant_type", "refresh_token")
    ]

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
deauthorize :: Client -> IO (Result DeauthorizationResponse)
deauthorize client = post client resource query
 where
  resource = "oauth/deauthorize"
  query = [] :: Query
