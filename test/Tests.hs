{-#LANGUAGE TemplateHaskell #-}

module Main where
import Test.QuickCheck
import Test.QuickCheck.All
import GameData
import AI
import Data.List (delete, sort)
import GameEngine
import System.Random (mkStdGen)

instance Arbitrary Card where
    arbitrary = elements universe

instance Arbitrary Suit where
    arbitrary = elements [Spades, Hearts, Clubs, Diamonds]
    
instance Arbitrary GameState where
    arbitrary = do
        p1HandSize <- choose (6, 10)
        p1KnownHandSize <- choose (0, 6)
        p2HandSize <- choose (6, 10)
        p2KnownHandSize <- choose (0, 6)
        discardPileSize <- choose (0, 35 - (p1HandSize + p2HandSize)) 
         
        deck <- testShuffle universe
        
        let p1Hand  = take p1HandSize deck
        let p2Hand  = take p2HandSize $ drop p1HandSize deck
        let p1Known = take p1KnownHandSize p1Hand
        let p2Known = take p2KnownHandSize p2Hand
        let discard = take discardPileSize $ drop (p1HandSize + p2HandSize) deck
        let remain  = drop (p1HandSize + p2HandSize + discardPileSize) deck        
         
        return $ GameState p1Hand p1Known p2Hand p2Known (last remain) remain discard emptyTransientState

instance Arbitrary PlayerVisibleState where
    arbitrary = do
        isPlayer1 <- arbitrary
        gameState <- arbitrary
        return $ preparePVS gameState isPlayer1
     
testShuffle :: (Eq a) => [a] -> Gen [a]
testShuffle [] = return []
testShuffle xs = do x <- oneof $ map return xs
                    ys <- testShuffle $ delete x xs
                    return (x:ys)

-- Checking that beats is a strict partial order
prop_beatsTrans tr c1 c2 c3 = (beats tr c1 c2 && beats tr c2 c3) ==> beats tr c1 c3
prop_beatsStrict tr c1 c2   = not (beats tr c1 c2 && beats tr c2 c1)
prop_beatsAntiSym tr c1     = not (beats tr c1 c1)

-- If a card has the same suit and greater value then it has greater offense and defense values 
prop_values gs c1@(Card v1 s1) c2@(Card v2 s2) =
    (s1 == s2 && v1 >= v2) ==> defenseValue c1 gs >= defenseValue c2 gs
                            && offenseValue c1 gs >= offenseValue c2 gs
                            
-- If a card beats another card, it will have at least the same offense and defense values                           
prop_beatval gs@(PlayerVisibleState _ _ (Card _ trump) _ _ _ _) c1 c2 =
    beats trump c1 c2 ==> defenseValue c1 gs >= defenseValue c2 gs
                       && offenseValue c1 gs >= offenseValue c2 gs

prop_shuffle list = sort (GameEngine.shuffle (mkStdGen 42) list) == sort list

runTests :: IO Bool
runTests = $quickCheckAll

main = runTests 
