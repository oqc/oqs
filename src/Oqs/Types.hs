{-# LANGUAGE FlexibleInstances #-}

module Types (

  QRef
, qRef
, unQRef
, verse
, qRefToLineNr

, QRefRng
, qRefRng
, unQRefRng
, fstVerse
, lstVerse
, fstToRef
, verseList
, qRefRngToLineNrs

, chap

, QLines
, qLines
, unQLines
, fromQLines

) where

import Test.Hspec


-- For single verse references like: "2:256"
newtype QRef = MkQRef (Int, Int)
  deriving ( Eq, Ord )
instance Show QRef where
  show (MkQRef (c, v)) = show c ++ ":" ++ show v
instance HasChap QRef where
  chap (MkQRef (c, _)) = c

qRef :: Int -> Int -> QRef
qRef c v = MkQRef (c, v)
-- TODO: ad guards

unQRef :: QRef -> (Int, Int)
unQRef (MkQRef (c, v)) = (c, v)

verse :: QRef -> Int
verse (MkQRef (_, v)) = v

qRefToLineNr :: QRef -> Int
qRefToLineNr (MkQRef (c, v)) = chapterOffset c + v


-- For reference ranges like: "80:1-8"
newtype QRefRng = MkQRefRng (Int, (Int, Int))
  deriving ( Eq, Ord )
instance Show QRefRng where
  show (MkQRefRng (c, (v1, v2))) = show c ++ ":" ++ show v1 ++ "-" ++ show v2
instance HasChap QRefRng where
  chap (MkQRefRng (c, _)) = c

qRefRng :: Int -> (Int, Int) -> QRefRng
qRefRng c (v1, v2) = MkQRefRng (c, (v1, v2))
-- TODO: add guards

unQRefRng :: QRefRng -> (Int, (Int, Int))
unQRefRng (MkQRefRng (c, (v1, v2))) = (c, (v1, v2))

fstVerse :: QRefRng -> Int
fstVerse (MkQRefRng (_, (v1, _))) = v1

lstVerse :: QRefRng -> Int
lstVerse (MkQRefRng (_, (_, v2))) = v2

verseList :: QRefRng -> [Int]
verseList (MkQRefRng (_, (v1, v2))) = [v1..v2]

fstToRef :: QRefRng -> QRef
fstToRef (MkQRefRng (c, (v, _))) = qRef c v

qRefRngToLineNrs :: QRefRng -> [Int]
qRefRngToLineNrs (MkQRefRng (c, (v1, v2))) = map (chapterOffset c +) [v1..v2]


-- Because both QRef and QRefRng want to define the `chap` function
class HasChap a where
  chap :: a -> Int


-- Any kind of resource that has 6236 lines (originals, translations, commentaries, break styles, etc.)
newtype QLines a = MkQLines [a]

qLines :: [a] -> QLines a
qLines lines = MkQLines lines
-- TODO: add guards

unQLines :: QLines a -> [a]
unQLines (MkQLines ts) = ts

class QLinesSelector s where
  fromQLines :: QLines a -> s -> [a]
instance QLinesSelector Int     where fromQLines (MkQLines ls) n = [ls !! (n - 1)]
instance QLinesSelector [Int]   where fromQLines (MkQLines ls) ns = map ((!!) ls . subtract 1) ns
instance QLinesSelector QRef    where fromQLines (MkQLines ls) r = [ls !! (qRefToLineNr r - 1)]
instance QLinesSelector [QRef]  where fromQLines qls           rs = fromQLines qls $ map qRefToLineNr rs
instance QLinesSelector QRefRng where fromQLines qls           rr = fromQLines qls $ qRefRngToLineNrs rr



-- INTERNAL

chapterOffset :: Int -> Int
chapterOffset c = sum $ take c versesPerChapter

versesPerChapter = [
  -- Start with zero as the 0th chapter has no verses.
  -- Chapter 9 has 129 verses as per the QTF standard.
  0,   7,   286, 200, 176, 120, 165, 206, 75,  129,
  109, 123, 111, 43,  52,  99,  128, 111, 110, 98,
  135, 112, 78,  118, 64,  77,  227, 93,  88,  69,
  60,  34,  30,  73,  54,  45,  83,  182, 88,  75,
  85,  54,  53,  89,  59,  37,  35,  38,  29,  18,
  45,  60,  49,  62,  55,  78,  96,  29,  22,  24,
  13,  14,  11,  11,  18,  12,  12,  30,  52,  52,
  44,  28,  28,  20,  56,  40,  31,  50,  40,  46,
  42,  29,  19,  36,  25,  22,  17,  19,  26,  30,
  20,  15,  21,  11,  8,   8,   19,  5,   8,   8,
  11,  11,  8,   3,   9,   5,   4,   7,   3,   6,
  3,   5,   4,   5,   6 ]


-- SPECS

main = hspec $ do

  describe "qRefToLineNr" $ do
    it "convert references to line numbers" $
      -- refToLineNr (114, 6) == 6236
      foldr (&&) True $ map refEqualsLineNr $
        [ (qRef 1 1,   1)
        , (qRef 2 1,   8)
        , (qRef 3 1,   294)
        , (qRef 3 200, 493)
        , (qRef 31 6,  3475)
        , (qRef 114 6, 6236)
        ]
      where refEqualsLineNr (r, lineNr) = qRefToLineNr r == lineNr

