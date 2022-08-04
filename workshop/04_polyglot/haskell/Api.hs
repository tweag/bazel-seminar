{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Text
import Servant.API

newtype Limit = Limit Int

instance ToHttpApiData Limit where
  toQueryParam (Limit a) = pack $ show a

type LogsGet = "logs" :> QueryParam "limit" Limit :> Get '[JSON] [Text]

type LogsPost = "logs" :> Capture "message" Text :> Post '[JSON] Int

type Api = "api" :> (LogsGet :<|> LogsPost)

api :: Proxy Api
api = Proxy

apiVersion :: Text
apiVersion = "0.0.1"
