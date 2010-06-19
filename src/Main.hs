{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell #-}
module Main where

import Site ( site, setup )

import Snap.Http.Server ( httpServe )

#ifdef PRODUCTION
import StaticSnap ( loadSnapTH )
#else
import HintSnap ( loadSnapTH )
#endif

import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               []  -> 8000
               p:_ -> read p
      aLog = Just "log/access.log"
      eLog = Just "log/error.log"

  snap <- $(loadSnapTH 'setup 'site)

  httpServe "*" port "localhost" aLog eLog snap
