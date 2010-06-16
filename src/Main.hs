{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import HintSnap ( loadSnap )

import Site ( site, setup )

import Snap.Http.Server ( httpServe )

import System.Environment ( getArgs )


main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               []  -> 8000
               p:_ -> read p
      aLog = Just "log/access.log"
      eLog = Just "log/error.log"

  siteStatic <- fmap site setup
  siteSnap <- loadSnap "src" "Site" "siteWithSetup"

  httpServe "*" port "localhost" aLog eLog siteSnap
