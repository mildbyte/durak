import Data.List ((\\), delete, elem)
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Card = Card 
        { cardValue :: Int
        , cardSuit  :: Suit
        } deriving (Eq)
                
instance Show Card where
    show (Card value suit)
        | value `elem` [6..10] = show value ++ " " ++ show suit
        | value == 11 = "Jack " ++ show suit
        | value == 12 = "Queen " ++ show suit
        | value == 13 = "King " ++ show suit
        | value == 14 = "Ace " ++ show suit

instance Read Card where
    readsPrec _ str = [(doRead str, "")] where
        doRead str = case suit of
            'H' -> Card value Hearts
            'D' -> Card value Diamonds
            'C' -> Card value Clubs
            'S' -> Card value Spades
         where
            suit  = last str
            value = read (init str) :: Int

-- Beats is a strict partial order on cards.
beats :: Suit -> Card -> Card -> Bool
beats trump (Card v1 s1) (Card v2 s2) =
       s1 == s2 && v1 > v2 || s1 == trump && s2 /= trump
       
-- All possible cards in game
universe = [Card v s | v <- [6..14], s <- [Hearts, Diamonds, Clubs, Spades]]

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

data OffenseAction = Attack Card
                   | FinishAttack

data DefenseAction = Defend Card Card
                   | GiveUp

emptyTransientState = TransientState [] [] []

testState = GameState [Card 11 Hearts, Card 6 Diamonds, Card 8 Spades, Card 6 Spades, Card 14 Spades, Card 12 Diamonds] [] (Card 7 Hearts) [Card 9 Spades] 6

-- Infers a list of cards the opponent may have:
-- all possible cards 
-- - our hand - discard pile - face-up trump card - cards we know he has - transient state
possibleOpponentCards :: GameState -> TransientState -> [Card]
possibleOpponentCards (GameState p d t c _) (TransientState ia id aa) =
    universe \\ (t : p ++ d ++ c ++ ia ++ id ++ aa)

-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> GameState -> TransientState -> Double
cardFraction pred state@(GameState _ _ _ knownCards opHandSize) tstate =
    if knownLength == 0 then unknownEstimate
    else knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
    where
        knownWeight = knownLength / fromIntegral opHandSize
        knownFraction = (fromIntegral $ length $ filter pred knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate = (fromIntegral $ length $ filter pred possibleCards) / 
                          (fromIntegral $ length possibleCards)
        possibleCards = possibleOpponentCards state tstate
        
-- The expected value of the fraction of cards
-- the opponent has that can't beat this card
offenseValue :: Card -> GameState -> TransientState -> Double
offenseValue card state tstate =
    cardFraction (\c -> not (beats (cardSuit $ trumpCard state) c card)) state tstate
    
-- The expected value of the fraction of cards
-- the opponent has that this card can beat
defenseValue :: Card -> GameState -> TransientState -> Double
defenseValue card state tstate =
    cardFraction (beats (cardSuit $ trumpCard state) card) state tstate

-- Calculates the offense action
-- Things to consider:
-- - probability of defender taking the cards
-- - minimizing defender's hand value
-- - future value of our hand (if we have to take cards from
--   the deck, what will be the value of our hand?)
-- offenseAction :: GameState -> TransientState -> OffenseAction

-- Calculates the defense action
-- Things to consider:
-- - future value of our hand if we take the cards on desk
--   or cards in the deck after we successfully defend
-- - how close are we to the end of the game?
-- defenseAction :: GameState -> TransientState -> DefenseAction

-- Simulates the opponent putting a card on the table: removes it
-- from the known hand and decreases the number of cards the opponent has.
updateKnownOpponentHand :: GameState -> Card -> GameState
updateKnownOpponentHand gs@(GameState _ _ _ kh hs) card =
    gs {knownOpponentHand = delete card kh,
        opponentHandSize  = hs - 1}

-- Applies an offense action to the game state
-- The boolean parameter is whether it's our turn (did we perform the action?)
applyOffenseAction :: GameState -> TransientState -> Bool -> OffenseAction -> (GameState, TransientState)
applyOffenseAction gs (TransientState ia id aa) _ FinishAttack =
    (gs {discardPile = (discardPile gs) ++ ia ++ id}, emptyTransientState)
applyOffenseAction gs ts@(TransientState _ _ aa) True (Attack card) =
    (gs {playerHand = delete card (playerHand gs)}, ts {activeAttack = card:aa})
applyOffenseAction gs@(GameState _ _ _ knownOpHand opHandSize) ts@(TransientState _ _ aa) False (Attack card) = 
    (updateKnownOpponentHand gs card, ts {activeAttack = card:aa})

-- Applies a defense action to the game state
-- The boolean parameter is whether we performed the action.
applyDefenseAction :: GameState -> TransientState -> Bool -> DefenseAction -> (GameState, TransientState)
applyDefenseAction gs ts@(TransientState ia id aa) myTurn (Defend against with) =
    (if myTurn
        then gs {playerHand = delete with (playerHand gs)}
        else updateKnownOpponentHand gs with,
     ts {inactiveAttack  = against:ia,
         activeAttack    = delete against aa,
         inactiveDefense = with:id})
applyDefenseAction gs ts@(TransientState ia id aa) myTurn GiveUp =
    (if myTurn
        then gs {playerHand        = (playerHand gs) ++ newCards}
        else gs {knownOpponentHand = (knownOpponentHand gs) ++ newCards,
                 opponentHandSize  = (opponentHandSize gs) + length newCards},
     emptyTransientState)
    where newCards = ia ++ id ++ aa
