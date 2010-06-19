{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module HintSnap
    ( loadSnap
    )
where

import Data.ByteString.Char8
    ( ByteString
    , append
    , intercalate
    , length
    , pack
    )

import Data.List ( nub )

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar
    ( newMVar
    , newEmptyMVar
    , putMVar
    , readMVar
    , swapMVar
    , takeMVar
    )
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )

import Data.Time.Clock ( NominalDiffTime, diffUTCTime, getCurrentTime )

import Language.Haskell.Interpreter
    ( InterpreterError(..)
    , OptionVal(..)
    , as
    , errMsg
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

-- Assumes mtl is the only package installed with a conflicting
-- Control.Monad.Trans
loadSnap :: String -> [String] -> String -> IO (Snap ())
loadSnap sPath mNames action = do
    let interpreter = do
        unsafeSetGhcOption "-hide-package=mtl"
        set [ searchPath := [sPath] ]
        loadModules . nub $ mNames
        let allMods = "Prelude":"Snap.Types":"Control.Monad.Trans":mNames
        setImports . nub $ allMods
        interpret action (as :: Snap ())

    loadAction <- protectedActionEvaluator 3 $ runInterpreter interpreter

    return $ do
        eSnap <- liftIO loadAction
        case eSnap of
            Left err -> do
                let msg = format err
                modifyResponse $ setContentType "text/plain; charset=utf-8"
                               . setResponseStatus 500 "Internal Server Error"
                               . setContentLength (fromIntegral $ length msg)
                writeBS msg

            Right handler -> handler


format :: InterpreterError -> ByteString
format (UnknownError e)   =
    append "Unknown interpreter error:\r\n\r\n" $ pack e

format (NotAllowed e)     =
    append "Interpreter action not allowed:\r\n\r\n" $ pack e

format (GhcException e)   =
    append "GHC error:\r\n\r\n" $ pack e

format (WontCompile errs) =
    let formatted = intercalate "\r\n" . map pack . nub . map errMsg $ errs
    in append "Compile errors:\r\n\r\n" formatted


protectedActionEvaluator :: NominalDiffTime -> IO a -> IO (IO a)
protectedActionEvaluator minReEval action = do
    readerContainer <- newMVar []
    resultContainer <- newMVar Nothing
    return $ do
        existingResult <- readMVar resultContainer
        now <- getCurrentTime

        case existingResult of
            Just (val, ts) | diffUTCTime now ts < minReEval -> return val
            _ -> do
                reader <- newEmptyMVar
                readers <- takeMVar readerContainer

                when (null readers) $ do
                    forkIO $ do
                        result <- action
                        allReaders <- takeMVar readerContainer
                        finishTime <- getCurrentTime
                        swapMVar resultContainer $ Just (result, finishTime)
                        putMVar readerContainer []
                        mapM_ (flip putMVar result) allReaders
                    return ()

                putMVar readerContainer $ reader : readers
                takeMVar reader
