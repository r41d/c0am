{-# LANGUAGE TemplateHaskell #-}
module TestSuite where

import Test.HUnit
import Test.Framework -- (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.TH
import Test.Framework.Providers.HUnit

import C0AM hiding (main)


main = defaultMain [$(testGroupGenerator)]

case_47 = c0am_case "47"
case_48 = c0am_case "48"
case_49 = c0am_case "49"
case_50 = c0am_case "50"
case_vorlesung = c0am_case "vorlesung"
case_simplesample = c0am_case "simplesample"

c0am_case :: String -> Assertion
c0am_case name = do
  let c0file = "../c0samples/" ++ name ++ ".c0"
  let amfile = "../c0samples/" ++ name ++ ".am"
  c0prog <- readFile c0file
  amprog <- readFile amfile
  -- equal strings = correct c0 → AM. yup, i'm serious :D
  assertEqual (c0file++" -> "++amfile) amprog (c0am c0prog++"\n")


