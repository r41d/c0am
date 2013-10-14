module QcProperties where


import Test.QuickCheck

import C0Types
import C0AMtypes
import C0AMtrans
import C0AMformat


main = verboseCheckWith (stdArgs {maxSuccess = 1000}) prop_sumNOPs_shrink


prop_sumNOPs_sumNOPs' :: [Command] -> Property
prop_sumNOPs_sumNOPs' xs = property $ sumNOPs xs == sumNOPs' xs
--        where types = xs::[Command]

prop_sumNOPs_shrink :: [Command] -> Property
prop_sumNOPs_shrink xs = classify (lenshrink < len) "actually shrinked something" $
                         lenshrink <= len
                         where len = length xs
                               lenshrink = length (sumNOPs xs)


prop_Lit :: Int -> Property
prop_Lit x = collect ("signum " ++ show (signum x)) $
             (factortrans (FN x) empty) == [(E, LIT (BracketlessInt x))]



{----------------------------------------------------------}
{----------Just some test properties-----------------------}
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

prop_Insert :: [Int] -> Property
prop_Insert xs = collect ("length: " ++ show (length xs)) $
                 classify (null xs) "empty list" $
                 reverse (reverse xs) == xs
  where types = ( xs::[Int] )
{----------------------------------------------------------}

