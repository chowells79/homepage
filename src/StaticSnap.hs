{-# LANGUAGE TemplateHaskell #-}
module StaticSnap
    ( loadSnapTH
    )
where

import Language.Haskell.TH.Syntax
    ( Exp(..)
    , Name
    , Q
    )

import Prelude hiding ( init )

loadSnapTH :: Name -> Name -> Q Exp
loadSnapTH init action = do
    let initE = VarE init
        actE = VarE action
        fmapE = VarE 'fmap
        simpleLoad = AppE (AppE fmapE actE) initE
    return simpleLoad
