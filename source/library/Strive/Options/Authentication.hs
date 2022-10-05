-- | 'Strive.Actions.Authentication'
module Strive.Options.Authentication
  ( BuildAuthorizeUrlOptions(..)
  ) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.buildAuthorizeUrl'
data BuildAuthorizeUrlOptions = BuildAuthorizeUrlOptions
  { buildAuthorizeUrlOptions_approvalPrompt :: Bool
  , buildAuthorizeUrlOptions_readScope :: Bool
  , buildAuthorizeUrlOptions_readAllScope :: Bool
  , buildAuthorizeUrlOptions_profileReadAllScope :: Bool
  , buildAuthorizeUrlOptions_profileWriteScope :: Bool
  , buildAuthorizeUrlOptions_activityReadScope :: Bool
  , buildAuthorizeUrlOptions_activityReadAllScope :: Bool
  , buildAuthorizeUrlOptions_activityWriteScope :: Bool
  , buildAuthorizeUrlOptions_state :: String
  }
  deriving Show

instance Default BuildAuthorizeUrlOptions where
  def = BuildAuthorizeUrlOptions
    { buildAuthorizeUrlOptions_approvalPrompt = False
    , buildAuthorizeUrlOptions_readScope = False
    , buildAuthorizeUrlOptions_readAllScope = False
    , buildAuthorizeUrlOptions_profileReadAllScope = False
    , buildAuthorizeUrlOptions_profileWriteScope = False
    , buildAuthorizeUrlOptions_activityReadScope = False
    , buildAuthorizeUrlOptions_activityReadAllScope = False
    , buildAuthorizeUrlOptions_activityWriteScope = False
    , buildAuthorizeUrlOptions_state = ""
    }

instance QueryLike BuildAuthorizeUrlOptions where
  toQuery options =
    toQuery
      $ [ ( "approval_prompt"
          , unpack
            (toStrict
              (encode (buildAuthorizeUrlOptions_approvalPrompt options))
            )
          )
        , ("state", buildAuthorizeUrlOptions_state options)
        ]
      <> if null scopes then [] else [("scope", intercalate "," scopes)]
   where
    scopes = catMaybes
      [ if buildAuthorizeUrlOptions_readScope options
        then Just "read"
        else Nothing
      , if buildAuthorizeUrlOptions_readAllScope options
        then Just "read_all"
        else Nothing
      , if buildAuthorizeUrlOptions_profileReadAllScope options
        then Just "profile:read_all"
        else Nothing
      , if buildAuthorizeUrlOptions_profileWriteScope options
        then Just "profile:write"
        else Nothing
      , if buildAuthorizeUrlOptions_activityReadScope options
        then Just "activity:read"
        else Nothing
      , if buildAuthorizeUrlOptions_activityReadAllScope options
        then Just "activity:read_all"
        else Nothing
      , if buildAuthorizeUrlOptions_activityWriteScope options
        then Just "activity:write"
        else Nothing
      ]
