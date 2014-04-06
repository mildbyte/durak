module AI where

import Data.List ((\\), maximumBy)
import Data.Function (on)

import GameData
import MiniMaxSearch

nothing :: a -> IO()
nothing _ = return()

aiPlayer :: Player
aiPlayer = Player daWrapper oaWrapper nothing nothing

oaWrapper gs actions = return $ chooseOffenseAction gs actions
daWrapper gs actions = return $ chooseDefenseAction gs actions

-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> PlayerVisibleState -> Double
cardFraction p
  state@(PlayerVisibleState _ _ _ _ knownCards opHandSize _ _)
  | knownLength == 0 = unknownEstimate
  | null possibleCards = knownFraction
  | otherwise =
    knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
  where knownWeight = knownLength / fromIntegral opHandSize
        knownFraction
          = fromIntegral (length $ filter p knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate
          = fromIntegral (length $ filter p possibleCards) /
              fromIntegral (length possibleCards)
        possibleCards = unseenCards state
        
-- The expected value of the fraction of cards
-- the opponent has that can't beat this card
offenseValue :: Card -> PlayerVisibleState -> Double
offenseValue card state =
    cardFraction (\c -> not (beats (cardSuit $ trumpCard state) c card)) state
    
-- The expected value of the fraction of cards
-- the opponent has that this card can beat
defenseValue :: Card -> PlayerVisibleState -> Double
defenseValue card state =
    cardFraction (beats (cardSuit $ trumpCard state) card) state

-- Weight for the hand size when evaluating a hand: less than 7 is okay,
-- apply a penalty after that.
cardNumberMultiplier :: Int -> Double
cardNumberMultiplier n = if n <= 6 then 1.0 else 1.0 / (fromIntegral n - 6.0)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Evaluates a hand's defense value given the current state.
-- Also takes the number of unknown cards in the hand (for cards we take from the deck)
handValue :: PlayerVisibleState -> [Card] -> Int -> Double
handValue gs cs unknown = mean (map (`defenseValue` gs) cs) * cardNumberMultiplier (length cs + unknown)

-- Evaluates a hand's defense value after we finish the turn and
-- take up some cards from the deck by taking a weighted average of
-- hand value and deck value.
-- If there are not enough cards in the deck, we will take the trump as well,
-- so the function adjusts our hand and the weight accordingly.
futureHandValue :: PlayerVisibleState -> [Card] -> Double
futureHandValue gs cs
    | length cs >= 6 = hVal  -- don't need to take cards from the deck
    | otherwise = hvWeight * hVal + (1 - hvWeight) * dVal
    where hvWeight = if lensum > 6
                     then fromIntegral (length cs) / 6.0
                     else fromIntegral (length cs + 1) / fromIntegral lensum
          hVal     = if lensum > 6
                     then handValue gs cs (6 - length cs)
                     else handValue gs (trumpCard gs : cs) (remainingDeckSize gs - 1)  -- will take all cards from the deck
          dVal     = deckValue gs                                                      -- but one of them is known
          lensum   = length cs + remainingDeckSize gs

-- Evaluates the average value of the cards in the deck
deckValue :: PlayerVisibleState -> Double
deckValue gs = mean $ map (`defenseValue` gs) $ unseenCards gs

-- Evaluates an offense action by considering our hand value after taking the cards from the deck.
evaluateOffenseAction :: PlayerVisibleState -> OffenseAction -> Double
evaluateOffenseAction gs FinishAttack = futureHandValue gs $ playerHand gs
evaluateOffenseAction gs (Attack cards) = futureHandValue gs $ playerHand gs \\ cards  

-- Evaluates a defense action by considering our hand value if we give up and take cards
-- or sacrifice our cards to defend against the attacking cards.
evaluateDefenseAction :: PlayerVisibleState -> DefenseAction -> Double
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ _ _ ts) GiveUp = futureHandValue gs (hand ++ allDeskCards ts)
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ _ _ _) (Defend cards) = futureHandValue gs (hand \\ map snd cards) 

-- Choosing an action for now is just about finding one with the maximum value.
-- If, otherwise, we can reconstruct the whole game state, we are in the endgame and can use
-- minimax to determine what to do.
chooseDefenseAction :: PlayerVisibleState -> [DefenseAction] -> DefenseAction
chooseDefenseAction gs = case constructSN gs of 
    Nothing   -> maximumBy (compare `on` evaluateDefenseAction gs)
    Just node -> minmaxDefense node

chooseOffenseAction :: PlayerVisibleState -> [OffenseAction] -> OffenseAction
chooseOffenseAction gs = case constructSN gs of
    Nothing   -> maximumBy (compare `on` evaluateOffenseAction gs)
    Just node -> minmaxAttack node