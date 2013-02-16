{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Prelude hiding ( readFile, lines, unlines )
import System ( getArgs )
import System.Exit
import System.IO ( stdout )
import qualified System.IO as S ( readFile )
import qualified Data.List as S ( lines )
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import Data.Text ( Text, lines, unlines, pack, unpack )
import Data.Text.IO
import Data.Maybe ( listToMaybe )

import Data.Aeson ( FromJSON, encode, object, (.=) )
{- import Data.Aeson.Encode.Pretty ( prettyEncode )-}
import qualified Data.ByteString.Lazy.Char8 as BL

import Opts
import Types
import RefParser


main = do
  args <- getArgs
  (opts, refString, files) <- optsParser args

  case parseRefRngs refString of

    Left errs -> do
      print $ show errs
      printUsageInfo
      exitWith $ ExitFailure 100

    Right refRngs -> do
      when (optVerbose opts) $ print (opts, refRngs, files)

      qtfs <- readQtfFiles files
      qpf  <- readQpfFile  (optQpfFile opts)

      let result = map (\refRng -> (show refRng, map (\qtf -> rngToQlf qpf (optBrkStyle opts) qtf refRng) qtfs))
                       (concat $ map (applyGrpStyle (optGrpStyle opts) qpf) refRngs)

      BL.putStrLn $ (encode . object) (map (\(k, v) -> (pack k) .= v) result)

      exitWith $ ExitSuccess


readQtfFiles :: [String] -> IO [QLines Text]  -- TODO: handle IO and parse exceptions
readQtfFiles files = mapM readFile files >>= mapM (qLines . lines) >>= return

readQpfFile :: String -> IO (QLines Int)
readQpfFile qpfFileName = do
  qpf <- S.readFile qpfFileName  -- TODO: handle exceptions, like "no valid qpf file" or "file non existant"
  qLines $ map (\s -> readNumOrZero [head s]) (S.lines qpf) >>= return
  where
    readNumOrZero :: String -> Int
    readNumOrZero s = case (fmap fst . listToMaybe . reads) s of Just i -> i; Nothing -> 0


rngToQlf :: QLines Int -> Bool -> QLines Text -> QRefRng -> Text
rngToQlf qpf brkStyle qtf refRng = T.concat $
  weave3 (map (\rng -> pack $ "\\nr{" ++ show rng ++ "} ") $ splitRngByVerses refRng)
         (fromQLines qtf refRng)
         (map (brkToText . head . fromQLines qpf) $ (init . splitRngByVerses) refRng)
  where
    weave3 :: [a] -> [a] -> [a] -> [a]
    weave3 []     _      _      = []  -- ys and zs are woven into xs, so empty
    weave3 xs     []     _      = xs  -- zs needs ys to be woven into xs
    weave3 (x:xs) (y:ys) []     = x:y   : weave3 xs ys []
    weave3 (x:xs) (y:ys) (z:zs) = x:y:z : weave3 xs ys zs

    brkToText :: Int -> Text
    brkToText brk = case brk of 0 -> " "
                                1 -> "\\br "
                                2 -> "\\bbr "
                                _ -> "\\bbr "

applyGrpStyle :: GrpStyle -> QLines Int -> QRefRng -> [QRefRng]
applyGrpStyle grpStyle qpf refRng = case grpStyle of
  RefRangesOnly -> [refRng]
  BigBreaks     -> splitRngByPars (1 <) qpf refRng
  AllBreaks     -> splitRngByPars (0 <) qpf refRng
  ByVerse       -> splitRngByVerses refRng



{- Feature ideas:
 -  * continuation marker when >300 verses are requested
 -}
