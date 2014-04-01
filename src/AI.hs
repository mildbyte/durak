module AI where

import Data.List ((\\), maximumBy)
import Data.Function (on)

import GameData

data AIPlayer = AIPlayer
instance Player AIPlayer where
    getOffenseAction _ gs actions = return $ chooseOffenseAction gs actions
    getDefenseAction _ gs actions = return $ chooseDefenseAction gs actions

-- Returns a list of cards that we haven't yet seen in the game
-- = universe of cards
-- - our hand - discard pile - face-up trump card - cards we know the opponent has - transient state
unseenCards :: PlayerVisibleState -> [Card]
unseenCards (PlayerVisibleState p d t c _ ts) =
    universe \\ (t : p ++ d ++ c ++ allDeskCards ts)

-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> PlayerVisibleState -> Double
cardFraction p state@(PlayerVisibleState _ _ _ knownCards opHandSize _) =
    if knownLength == 0 then unknownEstimate
    else knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
    where
        knownWeight = knownLength / fromIntegral opHandSize
        knownFraction = fromIntegral (length $ filter p knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate = fromIntegral (length $ filter p possibleCards) / 
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
cardNumberMultiplier n = if n <= 6 then 1.0
                                   else 1.0 / (fromIntegral n - 6.0)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Evaluates a hand's defense value given the current state.
handValue :: PlayerVisibleState -> [Card] -> Double
handValue gs cs = mean (map (`defenseValue` gs) cs) * cardNumberMultiplier (length cs)

-- Evaluates a hand's defense value after we finish the turn and
-- take up some cards from the deck by taking a weighted average of
-- hand value and deck value
futureHandValue :: PlayerVisibleState -> [Card] -> Double
futureHandValue gs cs = if length cs >= 6 then hVal
    else hvWeight * hVal + (1 - hvWeight) * dVal
    where hvWeight = fromIntegral (length cs) / 6.0
          hVal = handValue gs cs
          dVal = deckValue gs

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
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ ts) GiveUp = futureHandValue gs (hand ++ allDeskCards ts)
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ _) (Defend cards) = futureHandValue gs (hand \\ map snd cards) 

-- Choosing an action for now is just about finding one with the maximum value.
chooseDefenseAction :: PlayerVisibleState -> [DefenseAction] -> DefenseAction
chooseDefenseAction gs = maximumBy (compare `on` evaluateDefenseAction gs)

chooseOffenseAction :: PlayerVisibleState -> [OffenseAction] -> OffenseAction
chooseOffenseAction gs = maximumBy (compare `on` evaluateOffenseAction gs)