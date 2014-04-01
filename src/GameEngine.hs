-- Structures for game data and manipulating it
module GameEngine where

import Data.List (delete)
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

-- Engine-visible game state
data GameState = GameState 
        { player1Hand   :: [Card]
        , takenByP1     :: [Card] -- Cards in P1's hand that they took when giving up defense
        , player2Hand   :: [Card]
        , takenByP2     :: [Card]
        , remainingDeck :: [Card]
        , discardPile   :: [Card]
        , deskState     :: TransientState
        } deriving (Show, Eq)

-- State during an attack or defense
data TransientState = TransientState
        { inactiveAttack  :: [Card] -- Cards that have been defended against
        , inactiveDefense :: [Card] -- Cards that have defended against cards
        , activeAttack    :: [Card] -- Cards the player has yet to defend against
        } deriving (Show, Eq)

emptyTransientState :: TransientState
emptyTransientState = TransientState [] [] []

allDeskCards :: TransientState -> [Card]
allDeskCards (TransientState ia ind aa) = ia ++ ind ++ aa

-- All information that the player can infer from the game state:
-- the cards he has, the cards that are not played anymore,
-- the face-up trump card, the cards that his opponent had
-- to take when they abandoned their defense.
data PlayerVisibleState = PlayerVisibleState
        { playerHand        :: [Card]
        , seenDiscardPile   :: [Card]
        , trumpCard         :: Card
        , knownOpponentHand :: [Card]
        , opponentHandSize  :: Int
        , transientState    :: TransientState
        } deriving (Show, Eq)

-- Constructs the state that will be visible by the player, hiding information
preparePVS :: GameState -> Bool -> PlayerVisibleState
preparePVS (GameState hand _ opHand koHand deck discard ds) True =
    PlayerVisibleState hand discard (last deck) koHand (length opHand) ds
preparePVS (GameState opHand koHand hand _ deck discard ds) False =
    PlayerVisibleState hand discard (last deck) koHand (length opHand) ds

data OffenseAction = Attack Card
                   | FinishAttack
                   deriving Show

data DefenseAction = Defend Card Card
                   | GiveUp
                   deriving Show

-- Applies an offense action to the game state
-- The boolean parameter is whether player 1 performed the action
applyOffenseAction :: GameState -> Bool -> OffenseAction -> GameState
applyOffenseAction gs@(GameState _ _ _ _ dp _ (TransientState ia ind _)) _ FinishAttack =
    gs {discardPile = dp ++ ia ++ ind}
applyOffenseAction gs@(GameState hand _ _ koHand _ _ ts@(TransientState _ _ aa)) True (Attack card) =
    gs {player1Hand = delete card hand, takenByP2 = delete card koHand, deskState = ts {activeAttack = card:aa}}
applyOffenseAction gs@(GameState _ koHand hand _ _ _ ts@(TransientState _ _ aa)) False (Attack card) =
    gs {player2Hand = delete card hand, takenByP1 = delete card koHand, deskState = ts {activeAttack = card:aa}}

-- Applies a defense action to the game state
-- The boolean parameter is whether player 1 performed the action.
applyDefenseAction :: GameState -> Bool -> DefenseAction -> GameState
applyDefenseAction gs@(GameState hand _ _ koHand _ _ ts@(TransientState ia ind aa)) True (Defend against with) =
    gs {player1Hand = delete with hand, takenByP1 = delete with koHand, 
        deskState= ts {inactiveAttack  = against:ia,
                       activeAttack    = delete against aa,
                       inactiveDefense = with:ind}} 
applyDefenseAction gs@(GameState _ koHand hand _ _ _ ts@(TransientState ia ind aa)) False (Defend against with) =
    gs {player2Hand = delete with hand, takenByP2 = delete with koHand, 
        deskState= ts {inactiveAttack  = against:ia,
                       activeAttack    = delete against aa,
                       inactiveDefense = with:ind}}
applyDefenseAction gs@(GameState hand _ _ koHand _ _ ts) True GiveUp =
    gs {player1Hand = hand ++ allDeskCards ts, takenByP2 = koHand ++ allDeskCards ts, deskState = emptyTransientState} 
applyDefenseAction gs@(GameState _ koHand hand _ _ _ ts) False GiveUp =
    gs {player2Hand = hand ++ allDeskCards ts, takenByP1 = koHand ++ allDeskCards ts, deskState = emptyTransientState} 

