{-# LANGUAGE TemplateHaskell #-}

module Testsuite where

import Data.Monoid

import Test.Framework -- (defaultMain, defaultMainWithOpts, testGroup)
--import Test.Framework.Options (TestOptions, TestOptions'(..))
--import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase, hUnitTestToTests)
import Test.Framework.TH


import Test.QuickCheck
import Test.HUnit

import QcProperties as Q hiding (main)
import HunitTests as H hiding (main)

tests = H.allC0amHUnitTestsTF : []
--main = defaultMain allTests
--main = defaultMain tests
main = mainWithQuickCheck 1000 tests


mainWithQuickCheck runs tests = defaultMainWithOpts tests run_opts
    where run_opts = (mempty::RunnerOptions) { ropt_test_options = Just test_opts }
          test_opts = (mempty::TestOptions) { topt_maximum_generated_tests = Just runs }

