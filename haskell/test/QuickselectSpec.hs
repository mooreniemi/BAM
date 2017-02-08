{-# LANGUAGE ScopedTypeVariables #-}
module QuickselectSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Quickselect

import           Data.List           (sort)
import qualified Data.Set            as Set
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

newtype UniqueNum = UniqueNum {getUniques :: Vector Int}
  deriving (Eq, Show, Ord)

instance Arbitrary UniqueNum where
  arbitrary = do
    nums <- arbitrary `suchThat` (not . Set.null)
    nums' <- shuffle (Set.toList nums)
    return . UniqueNum . V.fromList $ nums'

main :: IO ()
main = hspec spec

testSelect f = do
  it "returns Nothing for an empty vector" $
    f 1 V.empty `shouldBe` Nothing
  it "returns Nothing if k is less than 1" $ property $
    \k (UniqueNum xs) -> k < 1 ==> f k xs === Nothing
  it "returns Nothing if k is greater than vector length" $ property $
    \k (UniqueNum xs) -> k > V.length xs ==> f k xs === Nothing
  it "returns the element for a one-element vector" $ property $
    \x -> f 1 (V.singleton x) === Just x
  it "works for a small example" $ do
    let xs = V.fromList [7, 10, 4, 3, 20, 15]
    f 3 xs `shouldBe` Just 7
  it "functions the same as taking the (k-1)th element of sorted list" $ property $
    \(UniqueNum xs) (Positive k) -> k < V.length xs ==>
      f k xs === Just (sort (V.toList xs) !! (k-1))

spec :: Spec
spec = do
  describe "sortselect" $
    testSelect sortselect
  describe "quickselect" $
    testSelect quickselect
