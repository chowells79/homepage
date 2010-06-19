{-# LANGUAGE CPP #-}
module SnapLoader
    ( loadSnapTH
    )
where

import Language.Haskell.TH ( Name, ExpQ)

#ifdef PRODUCTION
import StaticSnap ( snapTH )
#else
import HintSnap ( snapTH )
#endif

loadSnapTH :: Name -> Name -> ExpQ
loadSnapTH = snapTH
