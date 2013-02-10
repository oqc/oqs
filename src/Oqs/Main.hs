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

import Opts
import Types
import RefParser


main = do
  args <- getArgs
  (opts, refString, files) <- optsParser args

  case parseRefRngs refString of

    Left errs -> do
      print errs
      printUsageInfo
      exitWith $ ExitFailure 100

    Right refRngs -> do
      when (optVerbose opts) $ print (opts, refRngs, files)

      qtfFiles <- mapM readFile files  -- TODO: handle exceptions
      let qtfs = map (qLines . lines) qtfFiles :: [QLines Text]

      qpfFile <- S.readFile (optQpfFile opts)  -- TODO: handle exception, handle "no qpf file"
      let qpf = qLines $ map readNumOrZero $ map (show . head) (S.lines qpfFile) :: QLines Int

      hPutStrLn stdout $ pack $ show $
         map (\refRng -> (show refRng, map (\qtf -> rngToQlf qpf (optBrkStyle opts) qtf refRng) qtfs))
             (concat $ map (applyGrpStyle (optGrpStyle opts) qpf) refRngs)

      exitWith $ ExitSuccess
  where
    readNumOrZero s = case (fmap fst . listToMaybe . reads) s of Just i -> i; Nothing -> 0


rngToQlf :: QLines Int -> Bool -> QLines Text -> QRefRng -> Text
rngToQlf qpf brkStyle qtf refRng = T.concat $
  weave3 (map (\rng -> pack $ "\\nr{" ++ (show $ fstToRef rng) ++ "} ") $ splitRngByVerse refRng)
         (fromQLines qtf refRng)
         (map (brkToText . head . fromQLines qpf) $ init (splitRngByVerse refRng))
  where
    weave3 :: [a] -> [a] -> [a] -> [a]
    weave3 []     _      _      = []  -- ys and zs are weaved into xs, so empty
    weave3 xs     []     _      = xs  -- zs needs ys to be weaved into
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
  BigBreaks     -> [refRng]  -- TODO implement
  AllBreaks     -> [refRng]  -- TODO implement
  ByVerse       -> splitRngByVerse refRng

splitRngByVerse :: QRefRng -> [QRefRng]
splitRngByVerse rr = map (\v -> qRefRng (chap rr) (v, v)) $ verseList rr



{- Feature ideas:
 -  * error when >300 verses are requested
 -  * JSON output
 -  * breaking style (from file or off)
 -  * inter text grouping by:
 -    * ref ranges (least grouping)
 -    * refs ranges and big breaks only
 -    * ref ranges and any break
 -    * individual verses (most grouping)
 -}
