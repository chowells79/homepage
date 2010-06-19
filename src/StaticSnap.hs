{-# LANGUAGE TemplateHaskell #-}
module StaticSnap
    ( snapTH
    )
where

import Language.Haskell.TH.Syntax
    ( Exp(..)
    , Name
    , Q
    )

import Prelude hiding ( init )

snapTH :: Name -> Name -> Q Exp
snapTH init action = do
    let initE = VarE init
        actE = VarE action
        fmapE = VarE 'fmap
        simpleLoad = AppE (AppE fmapE actE) initE
    return simpleLoad
