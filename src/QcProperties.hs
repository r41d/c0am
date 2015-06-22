module QcProperties where

import Test.QuickCheck

import qualified C0Types
import qualified C0AMtypes
import qualified C0AMtrans
import qualified C0AMformat


--main = mapM_ (quickCheckWith (stdArgs {maxSuccess = 1000}))

c0amTypesQuickCheckProperties = C0AMtypes.c0amTypesQuickCheckProperties
c0amTransQuickCheckProperties = C0AMtrans.c0amTransQuickCheckProperties
c0amFormatQuickCheckProperties = C0AMformat.c0amFormatQuickCheckProperties

