{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.RefString where

import Import

import Quran.RefParser

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List ( head )

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRefStringR :: Text -> Handler RepJson
getRefStringR refs = do
  case parseRefRngs refs of

    Left errs -> do
      invalidArgs ["Could not parse '" `T.append` refs] -- FIXME msg no visible in response

    Right refRngs -> do
      yesod <- getYesod

      qtfMap <- liftIO $ readIORef $ getQtfMap yesod
      paramTxtIDs <- lookupGetParam "t"
      let qtfs = case paramTxtIDs of
           Nothing -> [head $ M.elems qtfMap]
           Just xs -> map ((!) qtfMap) $ (T.split (== ',')) xs

      qpfMap <- liftIO $ readIORef $ getQpfMap yesod
      paramParID <- lookupGetParam "t"
      let qpf = case paramParID of
           Nothing -> head $ M.elems qpfMap
           Just x  -> qpfMap ! x

      paramGrpStyle <- lookupGetParam "g"
      let grpStyle = case paramGrpStyle of
           Nothing -> RefRangesOnly
           Just x  -> case x of "r" -> RefRangesOnly
                                "b" -> BigBreaks
                                "a" -> AllBreaks
                                _   -> ByVerse   -- option "g=v", but also the catch-all

      let result = map (\refRng -> (show refRng, map (\qtf -> qtfRngToQlf qpf defaultBrkToText qtf refRng) qtfs))
                       (concat $ map (applyGrpStyleToRng grpStyle qpf) refRngs)

      (jsonToRepJson . object) (map (\(k, v) -> (T.pack k) .= v) result)


-- getIDsR :: Text -> Handler RepJson

