{-# LANGUAGE OverloadedStrings #-}

module Handler.GithubWebHook where

import Import
import Data.List (head)
import Network.Wai
import Data.Conduit.List
import Control.Monad.Trans.Resource

-- type GitInfo = GitInfo Text

postGithubWebHookR :: Handler RepJson
postGithubWebHookR = do
  --jb <- parseJsonBody_
  --jb :: Handler App App  
  -- wr <- waiRequest
  -- b  <- liftio . runresourcet . consume . requestbody $ wr
  --if null $ fst r
  --  then do jsontoRepJson $ String "No body found in request"
  --  else do
  --    let t = fst . head . snd $ r
  return . RepJson . toContent . toJSON $ String "OK"
