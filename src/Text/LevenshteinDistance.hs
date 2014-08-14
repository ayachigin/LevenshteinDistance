module Text.LevenshteinDistance (levenshteinDistance) where


import Data.Array


-- |
-- Levenshtein Distance
-- Measure similarity between two lists.
--
-- >>> levenshteinDistance "hoge" "hoge"
-- 0
-- >>> levenshteinDistance [1, 2, 3] [2, 2, 3]
-- 1
levenshteinDistance :: Eq a => [a] -> [a] -> Int
levenshteinDistance as [] = length as
levenshteinDistance [] bs = length bs
levenshteinDistance as bs = table ! (lenA, lenB)
    where
      arrayA = fromList as
      arrayB = fromList bs
      table = makeArray ((0, 0), (lenA, lenB)) f
      lenA = length as
      lenB = length bs
      f (0, b) = b
      f (a, 0) = a
      f (a, b)
        | charA == charB = table ! (a-1, b-1)
        | otherwise = 1 + minimum [ remove
                                  , insert
                                  , replace]
          where
            remove = table ! (a-1, b)
            insert = table ! (a, b-1)
            replace = table ! (a-1, b-1)
            charA = arrayA ! (a-1)
            charB = arrayB ! (b-1)

fromList :: [a] -> Array Int a
fromList ls = array (0, length ls - 1) $ zip [0..] ls

makeArray :: Ix i => (i, i) -> (i -> e) -> Array i e
makeArray bnds f = array bnds [(i, f i) | i <- range bnds]
