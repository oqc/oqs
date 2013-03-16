{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Language.Fay.Yesod
import Text.Read
import Data.Data
#ifdef FAY
import FFI
#else
import Language.Fay.FFI
#endif

data Command = RollDie (Returns Text)
    deriving (Read, Typeable, Data)

