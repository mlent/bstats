module Spec
  ( suite
  ) where

import           Data.Map
import           Posts
import           Test.Tasty
import           Test.Tasty.HUnit

content :: String
content = "+++\ncategory=[\"travel\"]\n+++\nThis is my body."

suite :: TestTree
suite =
  testGroup
    "Posts"
    [ testGroup
        "extractFrontmatter"
        [testCase "extracts the frontmatter from a post" extractFrontmatter1]
    , testGroup
        "extractPostBody"
        [testCase "extracts the body from a post" extractPostBody1]
    , testGroup
        "countWords"
        [testCase "count number of occurrences of each word" countWords1]
    , testGroup
        "sortWords"
        [testCase "sort a list of counted words by frequency" sortWords1]
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

sortWords1 :: Assertion
sortWords1 = expected @=? actual
  where
    actual = sortWords [("bananas", 1), ("apples", 2), ("peaches", 10)]
    expected = [("peaches", 10), ("apples", 2), ("bananas", 1)]
