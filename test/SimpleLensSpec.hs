{-# language NoMonomorphismRestriction #-}
-- the above courtesy of user "Wobben" on Discord. Needed for `lens` usage.

-- module SimpleLensSpec where

import           SimpleLens
import           Test.Hspec
import qualified Control.Lens.Tuple            as L
                                                ( _1
                                                , _2
                                                )
import qualified Control.Lens.Getter           as L
                                                ( view )
-- import qualified Control.Lens.Combinators
import qualified Control.Lens.Setter           as L
                                                ( set
                                                , over
                                                )

spec = do
  describe "prerequisite functors" $ do
    describe "Identity functor" $ it "has a working fmap" $ do
      (+ 1) <$> Identity 5 `shouldBe` Identity 6
      (++ "!") <$> Identity "hi" `shouldBe` Identity "hi!"
    describe "Const functor" $ it "has a working fmap" $ do
      (+ 1) <$> Const 5 `shouldBe` Const 5
      (++ "!") <$> Const "hi" `shouldBe` Const "hi"
  describe "Lenses" $ do
    describe "utilities" $ do
      describe "view" $ it "extracts a focus from a source" $ do
        view L._1 ("hi", 5) `shouldBe` "hi"
        view L._2 ("hi", 5) `shouldBe` 5
      describe "over" $ it "updates a focus in a source" $ do
        over L._1 head ("hi", 5) `shouldBe` ('h', 5)
        over L._2 even ("hi", 5) `shouldBe` ("hi", False)
      describe "set" $ it "sets a new focus in a source" $ do
        set L._1 True ("hi", 5) `shouldBe` (True, 5)
        set L._2 True ("hi", 5) `shouldBe` ("hi", True)
    describe "examples" $ do
      describe "tuples" $ do
        describe "_1" $ it "focuses on the first element of a 2-tuple" $ do
          L.view _1 ("hi", 5) `shouldBe` "hi"
          L.over _1 head ("hi", 5) `shouldBe` ('h', 5)
          L.set _1 True ("hi", 5) `shouldBe` (True, 5)
        describe "_2" $ it "focuses on the second element of a 2-tuple" $ do
          L.view _2 ("hi", 5) `shouldBe` 5
          L.over _2 even ("hi", 5) `shouldBe` ("hi", False)
          L.set _2 True ("hi", 5) `shouldBe` ("hi", True)
      describe "Person record"
        $ describe "_name"
        $ it "focuses on name field of a Person record"
        $ do
            L.view _name (Person "Jo" 30) `shouldBe` "Jo"
            L.over _name (++ "!") (Person "Jo" 30) `shouldBe` (Person "Jo!" 30)
            L.set _name "JJ" (Person "Jo" 30) `shouldBe` (Person "JJ" 30)
      describe "Temperatures"
        $ describe "_celsius"
        $ it "focuses on the celsius equivalent of a fahrenheit temp"
        $ do
            L.view _celsius (TempF 32) `shouldBe` (TempC 0)
            L.over _celsius (+ 5) (TempF 70) `shouldBe` (TempF 79)
            L.set _celsius 20 (TempF 99) `shouldBe` (TempF 68)
    describe "composition"
      $ describe "_1_1_1_name"
      $ it "focuses on the name of a person deeply nested in tuples"
      $ do
          L.view _1_1_1_name (((Person "K." 63, True), 9), 'm') `shouldBe` "K."
          L.over _1_1_1_name (++ "!") (((Person "K." 63, True), 9), 'm')
            `shouldBe` (((Person "K.!" 63, True), 9), 'm')
          L.set _1_1_1_name "G." (((Person "K." 63, True), 9), 'm')
            `shouldBe` (((Person "G." 63, True), 9), 'm')
    describe "`lens`" $ it "generates a lens from a getter and setter" $ do
      let _1' = lens fst (\(a, x) b -> (b, x))
      let _2' = lens snd (\(x, a) b -> (x, b))
      view _1' ("hi", 5) `shouldBe` "hi"
      over _1' head ("hi", 5) `shouldBe` ('h', 5)
      set _1' True ("hi", 5) `shouldBe` (True, 5)
      view _2' ("hi", 5) `shouldBe` 5
      over _2' even ("hi", 5) `shouldBe` ("hi", False)
      set _2' True ("hi", 5) `shouldBe` ("hi", True)

main = hspec spec
