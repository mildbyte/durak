import Data.List ((\\))

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
        , opponentHandSize :: Integer
        }

testState = GameState [Card 11 Hearts, Card 6 Diamonds, Card 8 Spades, Card 6 Spades, Card 14 Hearts, Card 12 Diamonds] [] (Card 7 Hearts) [Card 12 Spades] 6

-- Infers a list of cards the opponent may have:
-- all possible cards - our hand - discard pile - face-up trump card - cards we know he has
possibleOpponentCards :: GameState -> [Card]
possibleOpponentCards (GameState player discard trump known _) =
    universe \\ (trump : (player ++ discard ++ known))
    
-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> GameState -> Double
cardFraction pred state@(GameState _ _ _ knownCards opHandSize) =
    if knownLength == 0 then unknownEstimate
    else knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
    where
        knownWeight = knownLength / fromIntegral opHandSize
        knownFraction = (fromIntegral $ length $ filter pred knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate = (fromIntegral $ length $ filter pred possibleCards) / 
                          (fromIntegral $ length possibleCards)
        possibleCards = possibleOpponentCards state
        
-- The expected value of the fraction of cards
-- the opponent has that can't beat this card
offenseValue :: Card -> GameState -> Double
offenseValue card state =
    cardFraction (\c -> not (beats (cardSuit $ trumpCard state) c card)) state
    
-- The expected value of the fraction of cards
-- the opponent has that this card can beat
defenseValue :: Card -> GameState -> Double
defenseValue card state =
    cardFraction (beats (cardSuit $ trumpCard state) card) state