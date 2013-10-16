{-# LANGUAGE TemplateHaskell #-}
module HunitTests where

import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import System.IO

import C0AM hiding (main)

-- run with HUnit
main = runTestTT allC0amHUnitTests
allC0amHUnitTests = TestList $ map TestCase [case_47, case_48, case_49, case_50, case_vorlesung]

-- run with test-framework
allC0amHUnitTestsTF = $(testGroupGenerator)


case_47 = c0am_case "47"
case_48 = c0am_case "48"
case_49 = c0am_case "49"
case_50 = c0am_case "50"
case_vorlesung = c0am_case "vorlesung"

c0am_case :: String -> Assertion
c0am_case name = (do let c0file = "samples/" ++ name ++ ".c0"
                     let amfile = "samples/" ++ name ++ ".am"
                     c0prog <- readFile c0file
                     amprog <- readFile amfile 
                     assertEqual (c0file++" -> "++amfile) amprog (c0am c0prog++"\n")
                 )

