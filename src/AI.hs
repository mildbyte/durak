module AI where

import Data.List ((\\), delete, minimumBy, maximumBy, nub)
import Data.Function (on)

import GameEngine


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

-- Calculates the offense action
-- Things to consider:
-- - probability of defender taking the cards
-- - minimizing defender's hand value
-- - future value of our hand (if we have to take cards from
--   the deck, what will be the value of our hand?)
-- Current strategy: pick the card with the smallest defense value if first turn,
-- abandon attack if we can't find any cards to attack with, 
-- otherwise find the card such that if we put it on the table and take another one
-- from the deck, adding it to our hand, we get the maximum hand value.
-- If this maximum value is smaller than what we will get if we abandon the attack,
-- then abandon the attack, otherwise, attack with the card.
offenseAction :: PlayerVisibleState -> OffenseAction
offenseAction gs@(PlayerVisibleState _ _ _ _ _ ts)
    | ts == emptyTransientState = Attack $ pickFirstOffenseCard gs
    | possibleAttackCards /= [] && bestCardFV > futureHand = Attack bestCard
    | otherwise = FinishAttack
        where cardFV c = futureHandValue gs $ delete c $ playerHand gs
              bestCard = maximumBy (compare `on` cardFV) possibleAttackCards
              bestCardFV = cardFV bestCard
              futureHand = futureHandValue gs $ playerHand gs
              possibleAttackCards = filter (flip elem deskCardValues . cardValue) $ playerHand gs
              deskCardValues = map cardValue $ allDeskCards ts

-- Picks a card to start the offense with, the least-valuable card for defense for now
pickFirstOffenseCard :: PlayerVisibleState -> Card
pickFirstOffenseCard gs =
    minimumBy (\c1 c2 -> compare (defenseValue c1 gs) (defenseValue c2 gs)) (playerHand gs)

-- Calculates the defense action
-- Things to consider:
-- - future value of our hand if we take the cards on desk
--   or cards in the deck after we successfully defend
-- - how close are we to the end of the game?
defenseAction :: PlayerVisibleState -> [DefenseAction]
defenseAction gs@(PlayerVisibleState _ _ _ _ _ ts)
   | null validBeatings       = [GiveUp]
   | bestBeatingFV < giveUpFV = [GiveUp]
   | otherwise                = zipWith Defend (activeAttack ts) bestBeating
   where cardsBeatC ac = filter (\pc -> beats (cardSuit $ trumpCard gs) pc ac) $ playerHand gs
         beatingCards  = map cardsBeatC $ activeAttack ts
         validBeatings = filter (\xs -> nub xs == xs) $ sequence beatingCards
         giveUpFV      = handValue gs $ allDeskCards ts ++ playerHand gs
         beatingFV b   = handValue gs $ playerHand gs \\ b
         bestBeating   = maximumBy (compare `on` beatingFV) validBeatings
         bestBeatingFV = beatingFV bestBeating