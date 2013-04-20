module Main where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

import           Text.Web.EscapeTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        escapeTests
        ]
