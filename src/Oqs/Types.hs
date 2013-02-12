{-# LANGUAGE FlexibleInstances #-}

module Types (
  QRef
, qRef
, qRef_
, unQRef
, verse
, qRefToLineNr

, QRefRng
, qRefRng
, qRefRng_
, unQRefRng
, fstVerse
, lstVerse
, fstToRef
, verseList
, qRefRngToLineNrs
, splitRngByVerses
, splitRngByPars

, chap

, QLines
, qLines
, qLines_
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

qRef :: Monad m => Int -> Int -> m QRef
qRef c v  -- Programming errors here are no substitute for parser exceptions
  | c < 1 || c > 114 || v < 1 || v > versesPerChapter !! c =
                fail $ "Inexistant reference: " ++ (show $ MkQRef (c, v))
  | otherwise = return $ MkQRef (c, v)
qRef_ c v = MkQRef (c, v)

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
  show (MkQRefRng (c, (v1, v2))) = show c ++ ":" ++ show v1 ++ (if v1 == v2 then  ""  else  "-" ++ show v2)
instance HasChap QRefRng where
  chap (MkQRefRng (c, _)) = c

qRefRng :: Monad m => Int -> (Int, Int) -> m QRefRng
qRefRng c (v1, v2)  -- Programming errors here are no substitute for parser exceptions
  | c < 1 || c > 114 || v1 < 1 || v2 > versesPerChapter !! c =
                fail $ "Reference inexistant: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | v1 > v2   = fail $ "Start-verse cannot be larger than end-verse in reference: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | otherwise = return $ MkQRefRng (c, (v1, v2))
qRefRng_ c vs = MkQRefRng (c, vs)

unQRefRng :: QRefRng -> (Int, (Int, Int))
unQRefRng (MkQRefRng (c, (v1, v2))) = (c, (v1, v2))

fstVerse :: QRefRng -> Int
fstVerse (MkQRefRng (_, (v1, _))) = v1

lstVerse :: QRefRng -> Int
lstVerse (MkQRefRng (_, (_, v2))) = v2

verseList :: QRefRng -> [Int]
verseList (MkQRefRng (_, (v1, v2))) = [v1..v2]

fstToRef :: QRefRng -> QRef
fstToRef (MkQRefRng (c, (v, _))) = MkQRef (c, v)

qRefRngToLineNrs :: QRefRng -> [Int]
qRefRngToLineNrs (MkQRefRng (c, (v1, v2))) = map (chapterOffset c +) [v1..v2]

splitRngByVerses :: QRefRng -> [QRefRng]
splitRngByVerses rr = map (\v -> MkQRefRng (chap rr, (v, v))) $ verseList rr

splitRngByPars :: (Int -> Bool) -> QLines Int -> QRefRng -> [QRefRng]
splitRngByPars criterion qpf refRng =
  let markedPairs = zip (map criterion $ fromQLines qpf refRng)
                        (map (\x -> [x, x + 1]) $ verseList refRng)
  in map (\vs -> qRefRng_ (chap refRng) vs)
         (listToTuples ([fstVerse refRng] ++ (concat . (map snd) . (filter fst) . init $ markedPairs) ++ [lstVerse refRng]))
  where
    listToTuples :: [a] -> [(a, a)]
    listToTuples []       = []
    listToTuples [_]      = error "Odd lists should never occur"
    listToTuples (x:y:rs) = (x, y) : listToTuples rs


-- Because both QRef and QRefRng want to define the `chap` function
class HasChap a where
  chap :: a -> Int


-- Any kind of resource that has 6236 lines (originals, translations, commentaries, break styles, etc.)
newtype QLines a = MkQLines [a]
  deriving ( Show )

qLines :: Monad m => [a] -> m (QLines a)
qLines lines  -- Programming errors here are no substitute for parser exceptions
  | length lines /= 6236 = fail $ "The QTF format expects 6236 lines."
  | otherwise            = return $ MkQLines lines
qLines_ lines = MkQLines lines

unQLines :: QLines a -> [a]
unQLines (MkQLines ts) = ts

class QLinesSelector s where
  fromQLines :: QLines a -> s -> [a]
instance QLinesSelector Int     where fromQLines (MkQLines ls) n  = [ls !! (n - 1)]
instance QLinesSelector [Int]   where fromQLines (MkQLines ls) ns = map ((!!) ls . subtract 1) ns
instance QLinesSelector QRef    where fromQLines (MkQLines ls) r  = [ls !! (qRefToLineNr r - 1)]
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

  describe "qRef" $ do it "" $ pending

  describe "qRefToLineNr" $ do
    it "convert references to line numbers" $
      -- refToLineNr (114, 6) == 6236
      let refEqualsLineNr (r, lineNr) = qRefToLineNr r == lineNr
      in foldr (&&) True $ map refEqualsLineNr $
        [ (MkQRef (1, 1),   1)
        , (MkQRef (2, 1),   8)
        , (MkQRef (3, 1),   294)
        , (MkQRef (3, 200), 493)
        , (MkQRef (31, 6),  3475)
        , (MkQRef (114, 6), 6236)
        ]

  describe "qRefRng" $ do it "" $ pending
  describe "qRefRngToLineNrs" $ do it "" $ pending
  describe "splitRngByVerses" $ do it "" $ pending

  describe "splitRngByPars" $ do
    it "splits a ref range into smaller ranges as per paragraphing strategy" $
      foldr (&&) True $ map (\(crit, qpf, refRng, succ) -> succ == splitRngByPars crit qpf refRng) [
          ((0 <), MkQLines (take 6236 [0, 0..]), MkQRefRng (1, (1, 3)), [MkQRefRng (1, (1, 3))])
        , ((0 <), MkQLines (take 6236 [1, 1..]), MkQRefRng (1, (1, 3)), [MkQRefRng (1, (1, 1)), MkQRefRng (1, (2, 2)), MkQRefRng (1, (3, 3))])
        , ((0 <), MkQLines (take 6236 [1, 1..]), MkQRefRng (1, (5, 7)), [MkQRefRng (1, (5, 5)), MkQRefRng (1, (6, 6)), MkQRefRng (1, (7, 7))])
        , ((1 <), MkQLines (take 6236 [2, 2..]), MkQRefRng (2, (5, 7)), [MkQRefRng (2, (5, 5)), MkQRefRng (2, (6, 6)), MkQRefRng (2, (7, 7))])
        , ((0 <), MkQLines ([0] ++ take 6235 [1, 1..]), MkQRefRng (1, (1, 3)), [MkQRefRng (1, (1, 2)), MkQRefRng (1, (3, 3))])
        ]

      {- foldr (&&) True $ map (\(crit, qpf, refRng, succ) -> (show $ splitRngByPars crit qpf refRng) == succ) [-}
      {-     ((0 <), MkQLines (take 6236 [0, 0..]), MkQRefRng (1, (1, 3)), "[1:1-3]")-}
      {-   , ((0 <), MkQLines (take 6236 [1, 1..]), MkQRefRng (1, (1, 3)), "[1:1-1,1:2-2,1:3-3]")-}
      {-   , ((0 <), MkQLines (take 6236 [1, 1..]), MkQRefRng (1, (5, 7)), "[1:5-5,1:6-6,1:7-7]")-}
      {-   , ((1 <), MkQLines (take 6236 [2, 2..]), MkQRefRng (2, (5, 7)), "[2:5-5,2:6-6,2:7-7]")-}
      {-   , ((0 <), MkQLines ([0] ++ take 6235 [1, 1..]), MkQRefRng (1, (1, 3)), "[1:1-2,1:3-3]")-}
      {-   ]-}

  describe "qLines" $ do it "" $ pending



