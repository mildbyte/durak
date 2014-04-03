module GameEngine where

import GameData
import System.Random (StdGen, randoms)

-- Performs a turn in the game.
-- The turn starts with an attack: the first player chooses an attack from the generated list of
-- possible attacks. If the player doesn't want to attack anymore (can only happen after the first
-- attack), the turn ends and the game state is changed. Otherwise, it's the second player's turn
-- to defend: he chooses a defense action from a generated list, if he chooses to give up, the turn
-- is ended, if not, then it's again the first player's turn to respond.
-- Returns the final game state and a flag corresponding to whether the player successfully defended.

-- TODO: attack and defend seem to be very similar to each other, maybe there is a chance to merge them.
-- TODO: boolean flags to choose players seem crude.
attack :: Player -> Player -> GameState -> Bool -> IO(GameState, Bool)
attack p1 p2 gs isPlayer1 = do
    let (player, opp) = if isPlayer1 then (p1, p2) else (p2, p1)
    let playerState   = preparePVS gs isPlayer1
    playerChoice <- getOffenseAction player playerState (generateOffenseActions playerState)
    reactToOffense opp playerChoice
    let newState = applyOffenseAction gs isPlayer1 playerChoice

    case playerChoice of 
         FinishAttack -> return (newState, True)
         _            -> defend p1 p2 newState (not isPlayer1)

defend :: Player -> Player -> GameState -> Bool -> IO(GameState, Bool)
defend p1 p2 gs isPlayer1 = do
    let (player, opp) = if isPlayer1 then (p1, p2) else (p2, p1)
    let playerState   = preparePVS gs isPlayer1
    playerChoice <- getDefenseAction player playerState (generateDefenseActions playerState)
    reactToDefense opp playerChoice
    let newState = applyDefenseAction gs isPlayer1 playerChoice

    case playerChoice of 
         GiveUp -> return (newState, False)
         _      -> attack p1 p2 newState (not isPlayer1)
        

turn :: Player -> Player -> GameState -> Bool -> IO Bool
turn p1 p2 gs isPlayer1 =
    if gameOver gs then return $ null (player1Hand gs)
    else do
        (newGS, turnSwitch) <- attack p1 p2 gs isPlayer1
        let toppedUpGS = topUp (topUp newGS isPlayer1) (not isPlayer1)
        turn p1 p2 toppedUpGS (if turnSwitch then not isPlayer1 else isPlayer1)
              
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