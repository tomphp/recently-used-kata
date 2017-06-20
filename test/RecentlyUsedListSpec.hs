module RecentlyUsedListSpec where

import Test.Hspec
import Test.QuickCheck

import RecentlyUsedList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let list = add "second" ( add "first" emptyList)

  describe "getMostRecent" $ do
    it "it adds a single item to an empty list" $
      getMostRecent (add "example" emptyList) `shouldBe` Just "example"

    it "get most recent in an empty list returns nothing" $
      getMostRecent emptyList `shouldBe` Nothing

    it "returns the last added item first" $
      getMostRecent list `shouldBe` Just "second"

  describe "getByIndex" $ do
    it "gets the second item from the list" $
      getByIndex 1 list `shouldBe` Just "first"

    it "gets the first item from a list" $
      getByIndex 0 list `shouldBe` Just "second"

    it "return nothing if index is too large" $
      getByIndex 5 list `shouldBe` Nothing

    it "return nothing if index is negative" $
      getByIndex (-5) list `shouldBe` Nothing

  describe "add" $
    it "does not add duplicate items" $ do
      let newList = add "first" list
      getByIndex 0 newList `shouldBe` Just "first"
      getByIndex 1 newList `shouldBe` Just "second"
      getByIndex 2 newList `shouldBe` Nothing


