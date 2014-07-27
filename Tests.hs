module Main (
  main
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.ByteString.Lazy
import Data.Transit
import Data.Transit.JSON

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Encoding's reversibility"
    [ testProperty "Bools" (propRev :: [Bool] -> Bool)
    , testProperty "Nested" (propRev :: [[Bool]] -> Bool)
    ]
  ]

propRev x = decode JSON (encode JSON x :: ByteString) == Just x
