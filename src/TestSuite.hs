{-# LANGUAGE TemplateHaskell #-}
module TestSuite where

import Data.Monoid

import Test.QuickCheck
import Test.HUnit
import Test.Framework -- (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase, hUnitTestToTests)
import Test.Framework.TH

import QcProperties as Q hiding (main)
import HunitTests as H hiding (main)


props = [ Q.c0amTypesQuickCheckProperties
        , Q.c0amTransQuickCheckProperties
        , Q.c0amFormatQuickCheckProperties]

tests = [ H.allC0amHUnitTestsTF ]

main = mainWithQuickCheck 1000 (props ++ tests)

mainWithQuickCheck runs tests = defaultMainWithOpts tests run_opts
    where run_opts = (mempty::RunnerOptions) { ropt_test_options = Just test_opts }
          test_opts = (mempty::TestOptions) { topt_maximum_generated_tests = Just runs }

