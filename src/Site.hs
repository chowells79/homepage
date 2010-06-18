{-# LANGUAGE OverloadedStrings #-}
module Site where

import Control.Applicative ( (<$>) )
import Control.Monad ( msum )
import Control.Monad.Trans ( liftIO )

import Data.ByteString.Char8 ( ByteString, length, pack, unpack )

import Network.BSD ( getHostByAddr , hostName)
import Network.Socket ( Family(AF_INET), inet_addr )

import Prelude hiding ( length )

import Snap.Types
    ( Snap
    , getRequest
    , ifTop
    , ipHeaderFilter'
    , modifyResponse
    , rqRemoteAddr
    , setContentLength
    , setContentType
    , writeBS
    )

import Snap.Util.FileServe ( fileServe )

import Text.Templating.Heist
    ( TemplateState
    , bindStrings
    , renderTemplate
    , emptyTemplateState
    , loadTemplates
    )


frontPage :: TemplateState Snap -> Snap ()
frontPage ts = ifTop $ do
                 modifyResponse $ setContentType "text/html; charset=utf-8"

                 ip <- rqRemoteAddr <$> getRequest
                 hName <- liftIO $ safeHostFromIp ip

                 let ts' = bindStrings [ ("ip", ip), ("rdns", hName) ] ts

                 Just rendered <- renderTemplate ts' "index"
                 writeBS rendered
                 let len = fromIntegral . length $ rendered
                 modifyResponse . setContentLength $ len


safeHostFromIp :: ByteString -> IO ByteString
safeHostFromIp ip = resolve `catch` \_ -> return ip
    where resolve = do
            hostAddr <- inet_addr $ unpack ip
            host <- getHostByAddr AF_INET hostAddr
            return . pack . hostName $ host


staticResources :: Snap ()
staticResources = fileServe "resources/static"


site :: TemplateState Snap -> Snap ()
site ts = do
  ipHeaderFilter' "x-real-ip"

  msum [ frontPage ts
       , staticResources
       ]


setup :: IO (TemplateState Snap)
setup = do
  let ets = loadTemplates "resources/templates"
            (emptyTemplateState :: TemplateState Snap)
  either error id <$> ets
