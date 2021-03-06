-- Structures for game data and manipulating it
module GameData where

import Data.List ((\\), nub, groupBy, subsequences, sortBy, sort)
import Data.Function (on)
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Ord)
data Card = Card 
        { cardValue :: Int
        , cardSuit  :: Suit
        } deriving (Eq, Ord)
                
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
-- TODO: lists of cards have set semantics, move to sets?
data GameState = GameState 
        { player1Hand   :: [Card]
        , takenByP1     :: [Card] -- Cards in P1's hand that they took when giving up defense
        , player2Hand   :: [Card]
        , takenByP2     :: [Card]
        , gstrumpCard   :: Card   -- Trump card (also duplicated in the remaining deck)
        , remainingDeck :: [Card]
        , discardPile   :: [Card]
        , deskState     :: TransientState
        } deriving (Show, Ord)

instance Eq GameState where
    (==) (GameState p1Hand1 takenByP11 p2Hand1 takenByP21 trump1 remain1 dp1 ds1) 
         (GameState p1Hand2 takenByP12 p2Hand2 takenByP22 trump2 remain2 dp2 ds2) =
         all (uncurry ((==) `on` sort)) [(p1Hand1, p1Hand2), (takenByP11, takenByP12), 
                                         (p2Hand1, p2Hand2), (takenByP21, takenByP22),
                                         (remain1, remain2), (dp1, dp2)]
         && trump1 == trump2 && ds1 == ds2 
         

-- State during an attack or defense
data TransientState = TransientState
        { inactiveAttack  :: [Card] -- Cards that have been defended against
        , inactiveDefense :: [Card] -- Cards that have defended against cards
        , activeAttack    :: [Card] -- Cards the player has yet to defend against
        } deriving (Show, Ord)

instance Eq TransientState where
    (TransientState ina1 ind1 aa1) == (TransientState ina2 ind2 aa2) = all (uncurry ((==) `on` sort)) [(ina1, ina2), (ind1, ind2), (aa1, aa2)] 

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
        , playerTakenHand   :: [Card] -- Cards the player took when he gave up
        , seenDiscardPile   :: [Card]
        , trumpCard         :: Card
        , knownOpponentHand :: [Card]
        , opponentHandSize  :: Int
        , remainingDeckSize :: Int
        , transientState    :: TransientState
        } deriving (Show, Eq)

-- Returns a list of cards that we haven't yet seen in the game
-- = universe of cards
-- - our hand - discard pile - face-up trump card - cards we know the opponent has - transient state
unseenCards :: PlayerVisibleState -> [Card]
unseenCards (PlayerVisibleState p _ d t c _ _ ts) =
    universe \\ (t : p ++ d ++ c ++ allDeskCards ts)

-- Constructs the state that will be visible by the player, hiding information
preparePVS :: GameState -> Bool -> PlayerVisibleState
preparePVS (GameState hand tHand opHand koHand trump deck discard ds) True =
    PlayerVisibleState hand tHand discard trump koHand (length opHand) (length deck) ds
preparePVS (GameState opHand koHand hand tHand trump deck discard ds) False =
    PlayerVisibleState hand tHand discard trump koHand (length opHand) (length deck) ds

data OffenseAction = Attack [Card]
                   | FinishAttack
                   deriving (Show, Ord, Eq)

data DefenseAction = Defend [(Card, Card)]
                   | GiveUp
                   deriving (Show, Ord, Eq)



-- Applies an offense action to the game state
-- The boolean parameter is whether player 1 performed the action
applyOffenseAction :: GameState -> Bool -> OffenseAction -> GameState
applyOffenseAction gs@(GameState _ _ _ _ _ _ dp (TransientState ia ind _)) _ FinishAttack =
    gs {discardPile = dp ++ ia ++ ind, deskState = emptyTransientState}
applyOffenseAction gs@(GameState hand koHand _ _ _ _ _ ts@(TransientState _ _ aa)) True (Attack cards) =
    gs {player1Hand = hand \\ cards, takenByP1 = koHand \\ cards, deskState = ts {activeAttack = cards ++ aa}}
applyOffenseAction gs@(GameState _ _ hand koHand _ _ _ ts@(TransientState _ _ aa)) False (Attack cards) =
    gs {player2Hand = hand \\ cards, takenByP2 = koHand \\ cards, deskState = ts {activeAttack = cards ++ aa}}

-- Applies a defense action to the game state
-- The boolean parameter is whether player 1 performed the action.
applyDefenseAction :: GameState -> Bool -> DefenseAction -> GameState
applyDefenseAction gs@(GameState hand koHand _ _ _ _ _ ts@(TransientState ia ind aa)) True (Defend cards) =
    gs {player1Hand = hand \\ with, takenByP1 = koHand \\ with, 
        deskState= ts {inactiveAttack  = against ++ ia,
                       activeAttack    = aa \\ against,
                       inactiveDefense = ind ++ with}}
    where against = map fst cards
          with    = map snd cards 
applyDefenseAction gs@(GameState _ _ hand koHand _ _ _ ts@(TransientState ia ind aa)) False (Defend cards) =
    gs {player2Hand = hand \\ with, takenByP2 = koHand \\ with, 
        deskState= ts {inactiveAttack  = against ++ ia,
                       activeAttack    = aa \\ against,
                       inactiveDefense = ind ++ with}}
    where against = map fst cards
          with    = map snd cards
applyDefenseAction gs@(GameState hand koHand _ _ _ _ _ ts) True GiveUp =
    gs {player1Hand = hand ++ allDeskCards ts, takenByP1 = koHand ++ allDeskCards ts, deskState = emptyTransientState} 
applyDefenseAction gs@(GameState _ _ hand koHand _ _ _ ts) False GiveUp =
    gs {player2Hand = hand ++ allDeskCards ts, takenByP2 = koHand ++ allDeskCards ts, deskState = emptyTransientState}

-- Generates all possible defense actions:
-- if the player can't beat the current cards, give up
-- otherwise, for every card, get a list of cards that we have that can beat it,
-- take the Cartesian product of these lists and only keep the lists that represent a set.
-- Each of these list is a way to beat the current cards.
generateDefenseActions :: PlayerVisibleState -> [DefenseAction]
generateDefenseActions gs@(PlayerVisibleState _ _ _ _ _ _ _ ts) =
    GiveUp : map (Defend . zip (activeAttack ts)) validBeatings
    where cardsBeatC ac = filter (\pc -> beats (cardSuit $ trumpCard gs) pc ac) $ playerHand gs
          beatingCards  = map cardsBeatC $ activeAttack ts 
          validBeatings = filter (\xs -> nub xs == xs) $ sequence beatingCards

-- Generates all possible attack actions:
-- if we are opening the attack, take subsets of sets of cards that we have of equal value
-- (can attack with [7S] or [7S, 7C] etc
-- if we are continuing the attack, take all subsets of the set of cards we have whose values
-- are already on the desk.
generateOffenseActions :: PlayerVisibleState -> [OffenseAction]
generateOffenseActions (PlayerVisibleState hand _ _ _ _ opHandSize _ ts)
    | opHandSize == 0        = [FinishAttack]
    | null $ allDeskCards ts = map Attack  . filter ((>=) opHandSize . length) . filter (not . null) . concatMap subsequences $ 
        groupBy ((==) `on` cardValue) $ sortBy (compare `on` cardValue) hand -- group into equivalence classes on card values 
    | otherwise              = FinishAttack : (map Attack . filter (not . null) . subsequences $ filter (flip elem attackValues . cardValue) hand)  
    where attackValues = map cardValue $ allDeskCards ts


-- Game over if no cards remain in the deck and on the table and one of the players
-- has no cards.
gameOver :: GameState -> Bool
gameOver (GameState p1 _ p2 _ _ [] _ (TransientState [] [] [])) = null p1 || null p2
gameOver _ = False

-- Simulates a player taking cards from the deck
topUp :: GameState -> Bool -> GameState
topUp gs@(GameState p1 _ _ _ _ deck _ _) True =
    gs {player1Hand   = p1 ++ take number deck,
        remainingDeck = drop number deck} 
    where number = min (length deck) $ max (6 - length p1) 0
topUp gs@(GameState _ _ p2 _ _ deck _ _) False =
    gs {player2Hand   = p2 ++ take number deck,
        remainingDeck = drop number deck} 
    where number = min (length deck) $ max (6 - length p2) 0   

-- A player can react to game state with defensive or offensive actions.
-- The functions return monads in case the player is human-controlled.
data Player = Player {
                       getDefenseAction :: PlayerVisibleState -> [DefenseAction] -> IO DefenseAction
                     , getOffenseAction :: PlayerVisibleState -> [OffenseAction] -> IO OffenseAction
                     , reactToDefense   :: DefenseAction -> IO()
                     , reactToOffense   :: OffenseAction -> IO()
                     }