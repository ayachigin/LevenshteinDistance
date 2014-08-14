module LevenshteinDistanceSpec where

import Text.LevenshteinDistance (levenshteinDistance)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ()

-- newtype MyList (Eq a) = MyList a deriving (Eq, Show)

-- instance Arbitrary MyList where



spec :: Spec
spec = do
    describe "Levenshtein Distance" $ do
         {-
           prop "distance is 0 when called two same list" $ \(ls :: String) ->
           levenshteinDistance ls ls == 0
          -}
         prop ("distance between empty list and non-empty list is" ++
               "length of non-empty list") $ \ls ->
             levenshteinDistance "" ls == length ls
         prop "commutative" $ \ls ->
             levenshteinDistance ls "" == length ls
