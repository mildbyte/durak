module AI where

import Data.List ((\\), maximumBy, minimumBy)
import Data.Function (on)

import GameData

nothing :: a -> IO()
nothing _ = return()

aiPlayer :: Player
aiPlayer = Player daWrapper oaWrapper nothing nothing

oaWrapper gs actions = return $ chooseOffenseAction gs actions
daWrapper gs actions = return $ chooseDefenseAction gs actions

-- Reconstructs the complete game state at the endgame when the opponent's cards are known.
reconstructGS :: PlayerVisibleState -> Maybe GameState
reconstructGS pvs@(PlayerVisibleState myHand myTakenCards discard trump koHand _ 0 ts) =
    Just $ GameState myHand myTakenCards opponentHand koHand trump [] discard ts
    where opponentHand = unseenCards pvs ++ koHand 
reconstructGS _ = Nothing

-- Returns a list of cards that we haven't yet seen in the game
-- = universe of cards
-- - our hand - discard pile - face-up trump card - cards we know the opponent has - transient state
-- TODO: trump card sometimes is in the deck, sometimes not
unseenCards :: PlayerVisibleState -> [Card]
unseenCards (PlayerVisibleState p _ d t c _ _ ts) =
    universe \\ (t : p ++ d ++ c ++ allDeskCards ts)

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
-- TODO: small hands more valuable (after the deck runs out), but we would know the opponent's hand
-- by then; use minmax?
cardNumberMultiplier :: Int -> Double
cardNumberMultiplier n 
    | n == 6 = 1.0
    | n == 0 = 9000
    | n >  6 = 1.0 / (fromIntegral n - 6.0)
    | n <  6 = 1 + 1.0 / fromIntegral n

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

-- Performes a minimax search of the game tree when both opponents' hands are known.
-- Maximizes our score given that the other player will try and minimize our score.
-- TODO: what does "our score" mean? do we maximize the probability of wins?
-- TODO: terminates sometimes, but currently has been working for 5 mins, is there a cycle in the graph?
-- TODO: the code for all 4 cases is very similar, merge?
miniMaxEvalOffense :: GameState -> Bool -> OffenseAction -> Int
miniMaxEvalOffense gs isPlayer1 a@(Attack _) =
    let newState  = applyOffenseAction gs isPlayer1 a
        plState   = preparePVS newState (not isPlayer1)
        oppAction = miniMaxDefense newState (not isPlayer1) (generateDefenseActions plState)
    in miniMaxEvalDefense newState (not isPlayer1) oppAction
miniMaxEvalOffense gs isPlayer1 FinishAttack =
    let newState  = applyOffenseAction gs isPlayer1 FinishAttack
        plState   = preparePVS newState (not isPlayer1)
        oppAction = miniMaxOffense newState (not isPlayer1) (generateOffenseActions plState)
     in if gameOver newState then
        if null (player1Hand newState) then 1 else 0
        else miniMaxEvalOffense newState (not isPlayer1) oppAction

miniMaxEvalDefense :: GameState -> Bool -> DefenseAction -> Int
miniMaxEvalDefense gs isPlayer1 d@(Defend _) =
    let newState  = applyDefenseAction gs isPlayer1 d
        plState   = preparePVS newState (not isPlayer1)
        oppAction = miniMaxOffense newState (not isPlayer1) (generateOffenseActions plState)
    in miniMaxEvalOffense newState (not isPlayer1) oppAction
miniMaxEvalDefense gs isPlayer1 GiveUp =
    let newState  = applyDefenseAction gs isPlayer1 GiveUp
        plState   = preparePVS newState (not isPlayer1)
        oppAction = miniMaxOffense newState (not isPlayer1) (generateOffenseActions plState)
    in if gameOver newState then 
        if null (player1Hand newState) then 1 else 0
        else miniMaxEvalOffense newState (not isPlayer1) oppAction

miniMaxDefense :: GameState -> Bool -> [DefenseAction] -> DefenseAction
miniMaxDefense gs isPlayer1 = (if isPlayer1 then maximumBy else minimumBy) (compare `on` miniMaxEvalDefense gs isPlayer1)

miniMaxOffense :: GameState -> Bool -> [OffenseAction] -> OffenseAction
miniMaxOffense gs isPlayer1 = (if isPlayer1 then maximumBy else minimumBy) (compare `on` miniMaxEvalOffense gs isPlayer1)

-- Choosing an action for now is just about finding one with the maximum value.
-- If, otherwise, we can reconstruct the whole game state, we are in the endgame and can use
-- minimax to determine what to do.
chooseDefenseAction :: PlayerVisibleState -> [DefenseAction] -> DefenseAction
chooseDefenseAction gs = case reconstructGS gs of 
    Nothing  -> maximumBy (compare `on` evaluateDefenseAction gs)
    Just ags -> miniMaxDefense ags True

chooseOffenseAction :: PlayerVisibleState -> [OffenseAction] -> OffenseAction
chooseOffenseAction gs = case reconstructGS gs of
    Nothing  -> maximumBy (compare `on` evaluateOffenseAction gs)
    Just ags -> miniMaxOffense ags True