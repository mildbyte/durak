module AI where

import Data.List ((\\), delete, minimumBy, maximumBy, nub)
import Data.Function (on)

import GameEngine

-- The cards we have, the cards that are not played anymore,
-- the face-up trump card and the cards that the opponent had
-- to take when they abandoned their defense.
data GameState = GameState
        { playerHand :: [Card]
        , discardPile :: [Card]
        , trumpCard :: Card
        , knownOpponentHand :: [Card]
        , opponentHandSize :: Int
        } deriving (Show, Eq)

-- State during an attack or defense
data TransientState = TransientState
        { inactiveAttack  :: [Card] -- Cards that have been defended against
        , inactiveDefense :: [Card] -- Cards that have defended against cards
        , activeAttack    :: [Card] -- Cards the player has yet to defend against
        } deriving (Show, Eq)

allDeskCards :: TransientState -> [Card]
allDeskCards (TransientState ia ind aa) = ia ++ ind ++ aa

emptyTransientState :: TransientState
emptyTransientState = TransientState [] [] []

-- Returns a list of cards that we haven't yet seen in the game
-- = universe of cards
-- - our hand - discard pile - face-up trump card - cards we know the opponent has - transient state
unseenCards :: GameState -> TransientState -> [Card]
unseenCards (GameState p d t c _) (TransientState ia ind aa) =
    universe \\ (t : p ++ d ++ c ++ ia ++ ind ++ aa)

-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> GameState -> TransientState -> Double
cardFraction p state@(GameState _ _ _ knownCards opHandSize) tstate =
    if knownLength == 0 then unknownEstimate
    else knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
    where
        knownWeight = knownLength / fromIntegral opHandSize
        knownFraction = fromIntegral (length $ filter p knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate = fromIntegral (length $ filter p possibleCards) / 
                          fromIntegral (length possibleCards)
        possibleCards = unseenCards state tstate
        
-- The expected value of the fraction of cards
-- the opponent has that can't beat this card
offenseValue :: Card -> GameState -> TransientState -> Double
offenseValue card state =
    cardFraction (\c -> not (beats (cardSuit $ trumpCard state) c card)) state
    
-- The expected value of the fraction of cards
-- the opponent has that this card can beat
defenseValue :: Card -> GameState -> TransientState -> Double
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
handValue :: GameState -> TransientState -> [Card] -> Double
handValue gs ts cs = mean (map (\c -> defenseValue c gs ts) cs) * cardNumberMultiplier (length cs)

-- Evaluates a hand's defense value after we finish the turn and
-- take up some cards from the deck by taking a weighted average of
-- hand value and deck value
futureHandValue :: GameState -> TransientState -> [Card] -> Double
futureHandValue gs ts cs = if length cs >= 6 then hVal
    else hvWeight * hVal + (1 - hvWeight) * dVal
    where hvWeight = fromIntegral (length cs) / 6.0
          hVal = handValue gs ts cs
          dVal = deckValue gs ts

-- Evaluates the average value of the cards in the deck
deckValue :: GameState -> TransientState -> Double
deckValue gs ts = mean $ map (\c -> defenseValue c gs ts) $ unseenCards gs ts

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
offenseAction :: GameState -> TransientState -> OffenseAction
offenseAction gs ts
    | ts == emptyTransientState = Attack $ pickFirstOffenseCard gs emptyTransientState
    | possibleAttackCards /= [] && bestCardFV > futureHand = Attack bestCard
    | otherwise = FinishAttack
        where cardFV c = futureHandValue gs ts $ delete c $ playerHand gs
              bestCard = maximumBy (compare `on` cardFV) possibleAttackCards
              bestCardFV = cardFV bestCard
              futureHand = futureHandValue gs ts $ playerHand gs
              possibleAttackCards = filter (flip elem deskCardValues . cardValue) $ playerHand gs
              deskCardValues = map cardValue $ allDeskCards ts

-- Picks a card to start the offense with, the least-valuable card for defense for now
pickFirstOffenseCard :: GameState -> TransientState -> Card
pickFirstOffenseCard gs ts =
    minimumBy (\c1 c2 -> compare (defenseValue c1 gs ts) (defenseValue c2 gs ts)) (playerHand gs)

-- Calculates the defense action
-- Things to consider:
-- - future value of our hand if we take the cards on desk
--   or cards in the deck after we successfully defend
-- - how close are we to the end of the game?
defenseAction :: GameState -> TransientState -> [DefenseAction]
defenseAction gs ts
   | null validBeatings       = [GiveUp]
   | bestBeatingFV < giveUpFV = [GiveUp]
   | otherwise                = zipWith Defend (activeAttack ts) bestBeating
   where cardsBeatC ac = filter (\pc -> beats (cardSuit $ trumpCard gs) pc ac) $ playerHand gs
         beatingCards  = map cardsBeatC $ activeAttack ts
         validBeatings = filter (\xs -> nub xs == xs) $ sequence beatingCards
         giveUpFV      = handValue gs ts $ allDeskCards ts ++ playerHand gs
         beatingFV b   = handValue gs ts $ playerHand gs \\ b
         bestBeating   = maximumBy (compare `on` beatingFV) validBeatings
         bestBeatingFV = beatingFV bestBeating
        
-- Simulates the opponent putting a card on the table: removes it
-- from the known hand and decreases the number of cards the opponent has.
updateKnownOpponentHand :: GameState -> Card -> GameState
updateKnownOpponentHand gs@(GameState _ _ _ kh hs) card =
    gs {knownOpponentHand = delete card kh,
        opponentHandSize  = hs - 1}

-- Applies an offense action to the game state
-- The boolean parameter is whether it's our turn (did we perform the action?)
applyOffenseAction :: GameState -> TransientState -> Bool -> OffenseAction -> (GameState, TransientState)
applyOffenseAction gs (TransientState ia ind _) _ FinishAttack =
    (gs {discardPile = discardPile gs ++ ia ++ ind}, emptyTransientState)
applyOffenseAction gs ts@(TransientState _ _ aa) True (Attack card) =
    (gs {playerHand = delete card (playerHand gs)}, ts {activeAttack = card:aa})
applyOffenseAction gs ts@(TransientState _ _ aa) False (Attack card) = 
    (updateKnownOpponentHand gs card, ts {activeAttack = card:aa})

-- Applies a defense action to the game state
-- The boolean parameter is whether we performed the action.
applyDefenseAction :: GameState -> TransientState -> Bool -> DefenseAction -> (GameState, TransientState)
applyDefenseAction gs ts@(TransientState ia ind aa) myTurn (Defend against with) =
    (if myTurn
        then gs {playerHand = delete with (playerHand gs)}
        else updateKnownOpponentHand gs with,
     ts {inactiveAttack  = against:ia,
         activeAttack    = delete against aa,
         inactiveDefense = with:ind})
applyDefenseAction gs ts myTurn GiveUp =
    (if myTurn
        then gs {playerHand        = playerHand gs ++ newCards}
        else gs {knownOpponentHand = knownOpponentHand gs ++ newCards,
                 opponentHandSize  = opponentHandSize gs + length newCards},
     emptyTransientState)
    where newCards = allDeskCards ts
