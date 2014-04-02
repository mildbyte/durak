module GameEngine where

import GameData
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, randoms)
import Data.List (delete)

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

-- Performs a turn in the game.
-- The turn starts with an attack: the first player chooses an attack from the generated list of
-- possible attacks. If the player doesn't want to attack anymore (can only happen after the first
-- attack), the turn ends and the game state is changed. Otherwise, it's the second player's turn
-- to defend: he chooses a defense action from a generated list, if he chooses to give up, the turn
-- is ended, if not, then it's again the first player's turn to respond.
-- Returns the final game state and a flag corresponding to whether the player successfully defended.

-- TODO: AARGH UNSAFEPERFORMIO PUT THINGS INTO MONADS
-- TODO: attack and defend seem to be very similar to each other, maybe there is a chance to merge them.
-- TODO: boolean flags to choose players seem crude.
attack :: Player p => p -> p -> GameState -> Bool -> (GameState, Bool)
attack p1 p2 gs isPlayer1 = 
    case playerChoice of 
         FinishAttack -> (newState, True)
         _            -> defend p1 p2 newState (not isPlayer1)
    where player       = if isPlayer1 then p1 else p2
          playerState  = preparePVS gs isPlayer1
          possible     = generateOffenseActions playerState
          playerChoice = unsafePerformIO $ getOffenseAction player playerState possible -- AAARGH FIX FIX FIX
          newState     = applyOffenseAction gs isPlayer1 playerChoice

defend :: Player p => p -> p -> GameState -> Bool -> (GameState, Bool)
defend p1 p2 gs isPlayer1 =
    case playerChoice of 
         GiveUp    -> (newState, False)
         _         -> attack p1 p2 newState (not isPlayer1)
    where player       = if isPlayer1 then p1 else p2
          playerState  = preparePVS gs isPlayer1
          possible     = generateDefenseActions playerState
          playerChoice = unsafePerformIO $ getDefenseAction player playerState possible -- AAARGH FIX FIX FIX
          newState     = applyDefenseAction gs isPlayer1 playerChoice

turn :: Player p => p -> p -> GameState -> Bool -> Bool
turn p1 p2 gs isPlayer1 =
    if gameOver gs then null (player1Hand gs)
    else
        turn p1 p2 toppedUpGS (if turnSwitch then not isPlayer1 else isPlayer1)
        where (newGS, turnSwitch) = attack p1 p2 gs isPlayer1 
              toppedUpGS           = topUp (topUp newGS isPlayer1) (not isPlayer1)
              
shuffle' :: [Int] -> [a] -> [a]
shuffle' _ []          = []
shuffle' _ [x]         = [x]
shuffle' (i:is) (x:xs) = let s      = shuffle' is xs
                             (l, r) = splitAt (i `mod` (length xs + 1)) s 
                         in l ++ (x:r)
                         
shuffle :: StdGen -> [a] -> [a]
shuffle s = shuffle' $ randoms s

-- Generates an initial game state
generateGame :: StdGen -> GameState
generateGame gen = GameState p1 [] p2 [] (last remD) remD [] emptyTransientState
    where deck = shuffle gen universe
          p1   = take 6 deck
          p2   = take 6 $ drop 6 deck
          remD = drop 12 deck