{-# LANGUAGE OverloadedStrings #-}
module Site where

import Control.Applicative ( (<$>) )
import Control.Monad ( msum )
import Control.Monad.Trans ( liftIO )

import Control.Exception ( SomeException, catch )

import Data.ByteString.Char8 ( ByteString, pack, unpack )
import Data.Monoid
import qualified Data.Text as T

import Network.BSD ( getHostByAddr , hostName)
import Network.Socket ( Family(AF_INET), inet_addr )

import Prelude hiding ( catch )

import Snap.Core
    ( Snap
    , getRequest
    , ifTop
    , ipHeaderFilter'
    , modifyResponse
    , rqRemoteAddr
    , setContentType
    , writeBuilder
    )

import Snap.Util.FileServe ( serveDirectory )

import Text.Templating.Heist
    ( HeistState
    , bindStrings
    , renderTemplate
    , loadTemplates
    )


frontPage :: HeistState Snap -> Snap ()
frontPage ts = ifTop $ do
                 ip <- rqRemoteAddr <$> getRequest
                 hName <- liftIO $ safeHostFromIp ip

                 let ts' = bindStrings [ ("ip", t ip), ("rdns", t hName) ] ts
                     t = T.pack . unpack

                 Just (rendered, mimeType) <- renderTemplate ts' "index"
                 writeBuilder rendered
                 modifyResponse $ setContentType mimeType


safeHostFromIp :: ByteString -> IO ByteString
safeHostFromIp ip = resolve `catch` unchanged
  where
    unchanged :: SomeException -> IO ByteString
    unchanged _ = return ip

    resolve = do
        hostAddr <- inet_addr $ unpack ip
        host <- getHostByAddr AF_INET hostAddr
        return . pack . hostName $ host


staticResources :: Snap ()
staticResources = serveDirectory "resources/static"


makeSite :: HeistState Snap -> Snap ()
makeSite ts = do
  ipHeaderFilter' "x-real-ip"

  msum [ frontPage ts
       , staticResources
       ]


setup :: IO (HeistState Snap)
setup = do
  let ets = loadTemplates "resources/templates" (mempty :: HeistState Snap)
  either error id <$> ets
