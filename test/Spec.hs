module Spec
( suite
) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map
import Posts

content :: String
content = "+++ category=[\"travel\"] +++ This is my body."

suite :: TestTree
suite = testGroup "Posts"
  [
    testGroup "extractFrontmatter"
    [
      testCase "extracts the frontmatter from a post" extractFrontmatter1
    ]
  , testGroup "extractPostBody"
    [
      testCase "extracts the body from a post" extractPostBody1
    ]
  , testGroup "countWords"
    [
      testCase "count number of occurrences of each word" countWords1
    ]
  ]

extractFrontmatter1 :: Assertion
extractFrontmatter1 = expected @=? actual
  where
    actual = extractFrontmatter content
    expected = "category=[\"travel\"]"

extractPostBody1 :: Assertion
extractPostBody1 = expected @=? actual
  where
    actual = extractPostBody content
    expected = "This is my body."

countWords1 :: Assertion
countWords1 = expected @=? actual
  where
    actual = countWords "Apples apples bananas"
    expected = fromList [("apples", 2), ("bananas", 1)]
