module Quran.RefParser (
  parseRefs
) where

import Control.Monad (liftM)
import Control.Monad.Error
import Data.List (genericLength)
import Test.Hspec

parseRefs :: (Integral i, Read i) => String -> Either String [(i, i)]
parseRefs s = brokenChapterSplit s >>= toRefsFixingBrokenChapterSplit


-- INTERNAL

brokenChapterSplit :: String -> Either String [String]
brokenChapterSplit s = needsMinLengthOr 2 "No colon found." $ wordsWhen (== ':') s
  where needsMinLengthOr l error xs = if genericLength xs < l then Left error
                                                              else Right xs

toRefsFixingBrokenChapterSplit :: (Integral i, Read i) => [String] -> Either String [(i, i)]
toRefsFixingBrokenChapterSplit [] =
  Left "No references found"
toRefsFixingBrokenChapterSplit (chap:[]) =
  Left "Missing verse numbers. "
toRefsFixingBrokenChapterSplit (chap:vs:[]) =
  case verseStringToInts vs of
    Left e -> Left e
    Right ivs -> Right $ ungroup $ (read chap, ivs)
toRefsFixingBrokenChapterSplit (chap:vs:xs) =
  case verseStringToInts vs of
    Left e -> Left e
    Right ivs -> case toRefsFixingBrokenChapterSplit ((show $ last ivs):xs) of
                      Left e -> Left e
                      Right refs -> Right $ (ungroup (read chap, init ivs)) ++ refs

ungroup :: (a, [b]) -> [(a, b)]
ungroup (_, [])   = []
ungroup (c, v:vs) = (c, v):(ungroup (c, vs))

verseStringToInts :: (Integral i, Read i) => String -> Either String [i]
verseStringToInts str = liftM concat $ sequence $ map unrangeToInts $ wordsWhen (== ',') str

unrangeToInts :: (Integral i, Read i) => String -> Either String [i]
unrangeToInts s = case genericLength $ onHyphenToInts s of
                       0 -> Left "Missing verse number; double comma?"
                       1 -> Right $ onHyphenToInts s
                       2 -> Right $ [(onHyphenToInts s !! 0)..(onHyphenToInts s !! 1)]
                       _ -> Left "Unexpected hyphen; too many hypens?"
   where onHyphenToInts :: (Integral i, Read i) => String -> [i]
         onHyphenToInts s = map read $ wordsWhen (== '-') s


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen condition s =  case dropWhile condition s of
                              "" -> []
                              s' -> w : wordsWhen condition s''
                                    where (w, s'') = break condition s'


-- SPECS

main = hspec $ do

  describe "brokenChapterSplit" $ do
    it "should split a string on the colons" $ do
      brokenChapterSplit "2:256,31:6" == Right ["2", "256,31", "6"]

  describe "toRefsFixingBrokenChapterSplit" $ do
    it "should parse simple 'broken' ref strings correctly" $
      toRefsFixingBrokenChapterSplit ["2", "256"] == Right [(2,256)]
    it "should parse 'broken' ref strings correctly" $
      toRefsFixingBrokenChapterSplit ["1", "1,2,3,4,2", "256"] == Right [(1,1), (1,2), (1,3), (1,4), (2,256)]

  describe "unrangeToInts" $ do
    it "should parse single verses correctly" $
      unrangeToInts "19" == Right [19]
    it "should parse ranges correctly" $
      unrangeToInts "1-3" == Right [1,2,3]

  describe "parseRefs" $ do
    it "should parse complex ref strings appropriately" $
      parseRefs "1:1,2, 2:18, 3:100-103" == Right [(1,1), (1,2), (2,18), (3,100), (3,101), (3,102), (3,103)]
