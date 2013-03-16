{-# LANGUAGE CPP #-}
module Import
    ( module Import
    ) where


import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           Data.IORef           as Import (readIORef)
import           Data.Map             as Import (Map, (!))
import           Quran.Types          as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif



import Yesod.Fay
import Language.Haskell.TH.Syntax (Exp)
import System.Process (readProcess)
import Settings.Development

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName
    | development = fayFileReload settings
    | otherwise   = fayFileProd settings
  where
    settings = (yesodFaySettings moduleName)
        { yfsSeparateRuntime = Just ("static", staticR)
        -- , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        , yfsExternal = Just ("static", staticR)
        }

