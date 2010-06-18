{-# LANGUAGE TemplateHaskell, CPP #-}
module HintEnabler
    ( loadSnapTH
    )
where

#ifndef PRODUCTION
import Data.Maybe ( catMaybes )

import HintSnap ( loadSnap )

import System.Directory ( getCurrentDirectory )
import System.FilePath ( (</>) )
#endif

import Language.Haskell.TH.Syntax

import Prelude hiding ( init )

-- Assumes being spliced into the same source tree as the action to
-- dynamically load is located in
loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH init action = do
#ifdef PRODUCTION
    let initE = VarE init
        actE = VarE action
        fmapE = VarE 'fmap
        simpleLoad = AppE (AppE fmapE actE) initE
    return simpleLoad
#else
    loc <- location
    cwd <- runIO getCurrentDirectory

    let initMod = nameModule init
        initBase = nameBase init
        actMod = nameModule action
        actBase = nameBase action

        lf = length . loc_filename $ loc
        lm = length . loc_module $ loc
        relSrc = if lf > lm + 4
                   then take (lf - (lm + 4)) $ loc_filename loc
                   else "."
        src = cwd </> relSrc
        str = "liftIO " ++ initBase ++ " >>= " ++ actBase
        modules = catMaybes [initMod, actMod]

        loadSnapE = VarE 'loadSnap

    srcE <- lift src
    modulesE <- lift modules
    strE <- lift str

    return $ AppE (AppE (AppE loadSnapE srcE) modulesE) strE
#endif
