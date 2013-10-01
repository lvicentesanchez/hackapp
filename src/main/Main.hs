{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings #-}
module Main where

import Prelude                 hiding (head, id, (.))
import Control.Category        (Category(id, (.)))

import Control.Monad           (msum)
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson
import Data.Data               (Data, Typeable)
import Data.Text               (Text)
import GHC.Generics
import Happstack.Server        ( askRq, Response, ServerPartT, ok, toResponse
	                           , simpleHTTP, nullConf, seeOther, dir, notFound
	                           , seeOther, Method(POST), method, takeRequestBody, badRequest
	                           , unBody)
import Text.Boomerang.TH       (makeBoomerangs)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.Happstack    (implSite)
import Web.Routes.Boomerang

data User =
	User { name :: !Text
		 , age :: Int
		 } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Sitemap
    = UserOverview
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    ( lit "users" . rUserOverview
    )

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      UserOverview          -> msum[do method POST
                                       userOverviewPage
                               ]

userOverviewPage :: RouteT Sitemap (ServerPartT IO) Response
userOverviewPage = do
    requ <- askRq
    body <- liftIO $ takeRequestBody requ
    ubdy <- case body of 
                Just rqbody -> return . unBody $ rqbody 
                Nothing     -> return "" 
    case decode ubdy :: Maybe User of
        Just user -> ok $ toResponse $ encode $ user { age = age user * 2 }
        Nothing   -> badRequest $ toResponse $ ("Could not parse" :: String)

routeNotFound :: ServerPartT IO Response
routeNotFound = notFound $ toResponse $ ("Not found" :: String)

site :: Site Sitemap (ServerPartT IO Response)
site = boomerangSite (runRouteT route) sitemap

main :: IO ()
main = simpleHTTP nullConf $
       msum [ implSite "http://localhost:8000" "" site
            , routeNotFound
            ]
