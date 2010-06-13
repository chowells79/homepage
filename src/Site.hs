{-# LANGUAGE OverloadedStrings #-}
module Site where

import Control.Applicative ( (<$>) )
import Control.Monad ( msum )
import Control.Monad.Trans ( liftIO )

import Data.ByteString.Char8 ( length, pack, unpack )

import Network.BSD ( getHostByAddr , hostName)
import Network.Socket ( Family(AF_INET), inet_addr )

import Prelude hiding ( length )

import Snap.Types
    ( Snap
    , getHeader
    , getRequest
    , ifTop
    , modifyRequest
    , modifyResponse
    , rqRemoteAddr
    , setContentLength
    , setHeader
    , writeBS
    )

import Snap.Util.FileServe ( fileServe )

import Text.Templating.Heist
    ( TemplateState
    , bindStrings
    , renderTemplate
    )


frontPage :: TemplateState Snap -> Snap ()
frontPage ts = ifTop $ do
                 modifyResponse $ setHeader "Content-Type"
                                    "text/html; charset=utf-8"

                 let docType = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\r\n"
                 writeBS docType

                 ip <- rqRemoteAddr <$> getRequest
                 hostAddr <- liftIO . inet_addr $ unpack ip
                 let hostAct = getHostByAddr AF_INET hostAddr
                 hName <- pack . hostName <$> liftIO hostAct

                 let ts' = bindStrings [ ("ip", ip), ("rdns", hName) ] ts

                 Just rendered <- renderTemplate ts' "index"
                 writeBS rendered
                 let len = length docType + length rendered
                 modifyResponse $ setContentLength len


staticResources :: Snap ()
staticResources = fileServe "resources/static"


realIPFilter :: Snap ()
realIPFilter = do
  mRealIP <- getHeader "x-real-ip" <$> getRequest

  let setIP realIP = modifyRequest $ \rq -> rq { rqRemoteAddr = realIP }

  maybe (return ()) setIP mRealIP


site :: TemplateState Snap -> Snap ()
site ts = do
  realIPFilter

  msum [ frontPage ts
       , staticResources
       ]
