{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module HintSnap where

import Data.ByteString.Char8 ( length, pack )

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , newEmptyMVar
    , takeMVar
    , putMVar
    )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )

import Data.Typeable ( Typeable )

import Language.Haskell.Interpreter
    ( OptionVal(..)
    , as
    , set
    , searchPath
    , loadModules
    , interpret
    , runInterpreter
    , setImports
    )

import Language.Haskell.Interpreter.Unsafe ( unsafeSetGhcOption )

import Prelude hiding ( init, length )

import Snap.Types
    ( Snap
    , modifyResponse
    , setContentLength
    , setContentType
    , setHeader
    , setResponseStatus
    , writeBS
    )

loadSnap :: forall a. (Typeable a) => String -> [String] -> String ->
            String -> a -> IO (Snap ())
loadSnap sPath mNames aName iName _ = do
  let interpreter = do
        unsafeSetGhcOption "-hide-package=mtl"
        set [ searchPath := [sPath] ]
        loadModules mNames
        setImports [ "Prelude", "Site", "Snap.Internal.Types", "Text.Templating.Heist.Types" ]
        a <- interpret aName (as :: a -> Snap ())
        i <- interpret iName (as :: IO a)
        return (i, a)

  readInterpreter <- multiReader $ runInterpreter interpreter

  return $ do
    interpreterMVar <- liftIO $ readInterpreter
    eSnap <- liftIO . takeMVar $ interpreterMVar
    case eSnap of
      Left e -> do
          let err = pack . show $ e

          modifyResponse $ setContentType "text/plain; charset=utf-8"
                         . setResponseStatus 500 "Internal Server Error"
                         . setHeader "X-Internal-Error" err
                         . setContentLength (fromIntegral $ length err)

          writeBS err

      Right (init, handler) -> liftIO init >>= handler

multiReader :: IO a -> IO (IO (MVar a))
multiReader action = do
  readerContainer <- newMVar []
  return $ do
    reader <- newEmptyMVar
    readers <- takeMVar readerContainer

    when (null readers) $ do
      forkIO $ do
        result <- action
        readers' <- takeMVar readerContainer
        putMVar readerContainer []
        mapM_ (flip putMVar result) readers'
      return ()

    putMVar readerContainer $ reader : readers
    return reader
