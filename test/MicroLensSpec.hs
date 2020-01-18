module MicroLensSpec where

import           Prelude                 hiding ( sum )
import           MicroLens
import           Data.Monoid
import           Test.Hspec
import qualified Control.Lens                  as L

spec = do
  describe "Basic lensing" $ do
    it "should view _1" $ shouldBe (view _1 (1, 2)) 1
    it "should view _2" $ shouldBe (view _2 (1, 2)) 2
    it "should set _1" $ shouldBe (set _1 0 (1, 2)) (0, 2)
    it "should set _2" $ shouldBe (set _2 0 (1, 2)) (1, 0)
    it "should add" $ shouldBe (11, 2) $ _1 +~ 10 $ (1, 2)
    it "should return old integer" $ shouldBe (6, 7) $ ($) <<%~ (+ 1) $ 6
  describe "Basic traversals" $ do
    it "should traverse" $ shouldBe (toListOf elements [1, 2, 3]) [1, 2, 3]
    it "should miss" $ shouldBe (preview elements []) (Nothing :: Maybe Int)
  describe "Basic mapping" $ it "should map" $ shouldBe
    (toListOf (elements . to succ) [1, 2, 3])
    [2, 3, 4 :: Int]
  describe "Basic prisming" $ do
    it "should get Left"
      $ shouldBe (preview _Left (Left 3)) (Just 3 :: Maybe Int)
    it "should fail Right"
      $ shouldBe (preview _Left (Right 3)) (Nothing :: Maybe Int)
  describe "Isoing basic lenses" $ do
    it "should view _2" $ shouldBe (view (_flip . _1) (1, 2)) 2
    it "should view _1" $ shouldBe (view (_flip . _2) (1, 2)) 1
    it "should set _2" $ shouldBe (set (_flip . _1) 0 (1, 2)) (1, 0)
    it "should set _1" $ shouldBe (set (_flip . _2) 0 (1, 2)) (0, 2)

main :: IO ()
main = hspec spec
