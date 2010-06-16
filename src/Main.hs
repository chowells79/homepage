{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Snap.Http.Server ( httpServe )
import Snap.Types ( Snap )

import System.Environment ( getArgs )

import Text.Templating.Heist ( TemplateState )

import HintSnap ( loadSnap )
import Site ( site, setup )

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               []  -> 8000
               p:_ -> read p
      aLog = Just "log/access.log"
      eLog = Just "log/error.log"

      modules = [ "Site" ]

  siteStatic <- fmap site setup
  siteSnap <- loadSnap "src" modules "site" "setup"
              (undefined :: TemplateState Snap)

  httpServe "*" port "localhost" aLog eLog siteSnap
