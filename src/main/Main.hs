{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import           Control.Category       (Category ((.)))
import           Prelude                hiding ((.))

import           Control.Monad          (msum)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Data              (Data, Typeable)
import           Data.Text              (Text)
import           GHC.Generics
import           Happstack.Server       (Method (POST), Response, ServerPartT,
                                         askRq, badRequest, method, notFound,
                                         nullConf, ok, simpleHTTP,
                                         takeRequestBody, toResponse, unBody)
import           Text.Boomerang.TH      (makeBoomerangs)
import           Web.Routes             (RouteT, Site (..), runRouteT)
import           Web.Routes.Boomerang
import           Web.Routes.Happstack   (implSite)

data User =
    User { name :: !Text
         , age  :: Int
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
        UserOverview -> msum[do method POST
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
        Nothing   -> badRequest $ toResponse ("Could not parse" :: String)

routeNotFound :: ServerPartT IO Response
routeNotFound = notFound $ toResponse ("Not found" :: String)

site :: Site Sitemap (ServerPartT IO Response)
site = boomerangSite (runRouteT route) sitemap

main :: IO ()
main = simpleHTTP nullConf $
    msum [ implSite "http://localhost:8000" "" site
         , routeNotFound
    ]
