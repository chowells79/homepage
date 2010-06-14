{-# LANGUAGE ScopedTypeVariables #-}
module HintSnap where

import Data.ByteString.Char8 ( pack )

import Control.Monad.Trans ( liftIO )

import Data.Typeable ( Typeable )

import Language.Haskell.Interpreter
    ( OptionVal(..)
    , set
    , searchPath
    , loadModules
    , interpret
    , runInterpreter
    , setImports
    )

import Language.Haskell.Interpreter.Unsafe ( unsafeSetGhcOption )

import Prelude hiding ( init )

import Snap.Types ( Snap, writeBS )

loadSnap :: forall a. (Typeable a) => String -> [String] -> String ->
            IO a -> IO (Snap ())
loadSnap sPath mNames aName init = do
  let interpreter = do
        unsafeSetGhcOption "-hide-package=mtl"
        set [ searchPath := [sPath] ]
        loadModules mNames
        setImports [ "Site", "Snap.Internal.Types", "Text.Templating.Heist.Types" ]
        interpret aName (undefined :: a -> Snap ())

  state <- init

  return $ do
    eSnap <- liftIO . runInterpreter $ interpreter
    case eSnap of
      Left err -> writeBS . pack . show $ err
      Right makeSnap -> makeSnap state
