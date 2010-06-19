{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module HintSnap
    ( hintSnap
    , loadSnapTH
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

import Data.Maybe ( catMaybes )
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

import Language.Haskell.TH.Syntax
    ( Exp(..)
    , Name
    , Q
    , lift
    , location
    , loc_filename
    , loc_module
    , nameBase
    , nameModule
    , runIO
    )

import Prelude hiding ( init, length )
import qualified Prelude as P

import Snap.Types
    ( Snap
    , modifyResponse
    , setContentLength
    , setContentType
    , setResponseStatus
    , writeBS
    )

import System.Directory ( getCurrentDirectory )
import System.FilePath ( (</>) )

-- Assumes being spliced into the same source tree as the action to
-- dynamically load is located in
loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH init action = do
    loc <- location
    cwd <- runIO getCurrentDirectory

    let initMod = nameModule init
        initBase = nameBase init
        actMod = nameModule action
        actBase = nameBase action

        lf = P.length . loc_filename $ loc
        lm = P.length . loc_module $ loc
        relSrc = if lf > lm + 4
                   then take (lf - (lm + 4)) $ loc_filename loc
                   else "."
        src = cwd </> relSrc
        str = "liftIO " ++ initBase ++ " >>= " ++ actBase
        modules = catMaybes [initMod, actMod]

        hintSnapE = VarE 'hintSnap

    srcE <- lift src
    modulesE <- lift modules
    strE <- lift str

    return $ AppE (AppE (AppE hintSnapE srcE) modulesE) strE


-- Assumes mtl is the only package installed with a conflicting
-- Control.Monad.Trans
hintSnap :: String -> [String] -> String -> IO (Snap ())
hintSnap sPath mNames action = do
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
