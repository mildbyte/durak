{-#LANGUAGE TemplateHaskell #-}

module Main where
import Test.QuickCheck
import Test.QuickCheck.All
import GameEngine
import AI
import Data.List (delete)

instance Arbitrary Card where
    arbitrary = elements universe

instance Arbitrary Suit where
    arbitrary = elements [Spades, Hearts, Clubs, Diamonds]
    
testState :: GameState
testState = GameState [Card 11 Hearts, Card 6 Diamonds, Card 8 Spades, Card 6 Spades, Card 14 Spades, Card 12 Diamonds] [] (Card 7 Hearts) [Card 9 Spades] 6

instance Arbitrary GameState where
     arbitrary = do
         myHandSize <- choose (6, 10)
         foeKnownHandSize <- choose (0, 6)
         foeHandSize <- choose (foeKnownHandSize, 10)
         
         deck <- shuffle universe
         
         let myHand   = take myHandSize deck
         let trump    = head $ drop myHandSize deck
         let knownFoe = take foeKnownHandSize $ drop (myHandSize + 1) deck
         
         return $ GameState myHand [] trump knownFoe foeHandSize

shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do x <- oneof $ map return xs
                ys <- shuffle $ delete x xs
                return (x:ys)

prop_beatsTrans tr c1 c2 c3 = (beats tr c1 c2 && beats tr c2 c3) ==> beats tr c1 c3
prop_beatsStrict tr c1 c2   = not (beats tr c1 c2 && beats tr c2 c1)
prop_beatsAntiSym tr c1     = not (beats tr c1 c1)

-- If a card has the same suit and greater value then it has greater offense and defense values 
prop_values gs c1@(Card v1 s1) c2@(Card v2 s2) =
    (s1 == s2 && v1 >= v2) ==> defenseValue c1 gs emptyTransientState >= defenseValue c2 gs emptyTransientState
                            && offenseValue c1 gs emptyTransientState >= offenseValue c2 gs emptyTransientState
                            
-- If a card beats another card, it will have at least the same offense and defense values                           
prop_beatval gs@(GameState _ _ (Card _ trump) _ _) c1 c2 =
    beats trump c1 c2 ==> defenseValue c1 gs emptyTransientState >= defenseValue c2 gs emptyTransientState
                       && offenseValue c1 gs emptyTransientState >= offenseValue c2 gs emptyTransientState

runTests :: IO Bool
runTests = $quickCheckAll

main = runTests 
