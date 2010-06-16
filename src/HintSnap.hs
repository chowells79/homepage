{-# LANGUAGE OverloadedStrings #-}
module HintSnap
    ( loadSnap
    )
where

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
    , setResponseStatus
    , writeBS
    )

loadSnap :: String -> String -> String -> IO (Snap ())
loadSnap sPath mName aName = do
  let interpreter = do
        unsafeSetGhcOption "-hide-package=mtl"
        set [ searchPath := [sPath] ]
        loadModules [ mName ]
        setImports [ mName, "Prelude", "Snap.Types" ]
        interpret aName (as :: Snap ())

  readInterpreter <- multiReader $ runInterpreter interpreter

  return $ do
    eSnap <- liftIO $ readInterpreter >>= takeMVar
    case eSnap of
      Left e -> do
          let err = pack . show $ e

          modifyResponse $ setContentType "text/plain; charset=utf-8"
                         . setResponseStatus 500 "Internal Server Error"
                         . setContentLength (fromIntegral $ length err)

          writeBS err

      Right handler -> handler

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
