{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ( (<$>) )

import Snap.Http.Server ( httpServe )

import System.Environment ( getArgs )

import Snap.Types ( Snap )

import Text.Templating.Heist
    ( TemplateState
    , emptyTemplateState
    , loadTemplates
    )

import HintSnap ( loadSnap )
--import Site ( site )

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               []  -> 8000
               p:_ -> read p
      aLog = Just "log/access.log"
      eLog = Just "log/error.log"

      modules = [ "Site" ]--, "Text.Templating.Heist", "Snap.Types" ]

      ets = loadTemplates "resources/templates" (emptyTemplateState :: TemplateState Snap)
      ts = either error id <$> ets

  --site' <- site <$> ts
  site' <- loadSnap "src" modules "site" ts

  httpServe "*" port "localhost" aLog eLog site'
