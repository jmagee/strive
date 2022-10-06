{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Types.Authentication
  ( TokenExchangeResponse(..)
  , DeauthorizationResponse(..)
  , RefreshTokenResponse(..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteSummary)

-- | <http://strava.github.io/api/v3/oauth/#example-response>
data TokenExchangeResponse = TokenExchangeResponse
  { tokenExchangeResponse_accessToken :: Text
  , tokenExchangeResponse_athlete :: AthleteSummary
  , tokenExchangeResponse_expiresAt :: Integer
  , tokenExchangeResponse_expiresIn :: Integer
  , tokenExchangeResponse_refreshToken :: Text
  }
  deriving Show

$(deriveFromJSON options ''TokenExchangeResponse)

data RefreshTokenResponse = RefreshTokenResponse
  { refreshTokenResponse_accessToken :: Text
  , refreshTokenResponse_expiresAt :: Integer
  , refreshTokenResponse_expiresIn :: Integer
  , refreshTokenResponse_refreshToken :: Text
  }
  deriving Show

$(deriveFromJSON options ''RefreshTokenResponse)

-- | <http://strava.github.io/api/v3/oauth/#example-response-1>
data DeauthorizationResponse = DeauthorizationResponse
  { deauthorizationResponse_accessToken :: Text
  }
  deriving Show

$(deriveFromJSON options ''DeauthorizationResponse)
