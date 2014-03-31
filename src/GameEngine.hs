module GameEngine where

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
    readsPrec _ s = [(doRead s, "")] where
        doRead s = case suit of
            'H' -> Card value Hearts
            'D' -> Card value Diamonds
            'C' -> Card value Clubs
            'S' -> Card value Spades
         where
            suit  = last s
            value = read (init s) :: Int

-- Beats is a strict partial order on cards.
beats :: Suit -> Card -> Card -> Bool
beats trump (Card v1 s1) (Card v2 s2) =
       s1 == s2 && v1 > v2 || s1 == trump && s2 /= trump
       
-- All possible cards in game
universe :: [Card]
universe = [Card v s | v <- [6..14], s <- [Hearts, Diamonds, Clubs, Spades]]

data OffenseAction = Attack Card
                   | FinishAttack
                   deriving Show

data DefenseAction = Defend Card Card
                   | GiveUp
                   deriving Show
