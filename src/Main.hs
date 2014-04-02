module Main where

import System.Random (getStdGen)
import GameEngine
import HumanPlayer
import AI

main::IO()
main = do
    gen  <- getStdGen
    let game = generateGame gen
    let p1 = humanPlayer
    let p2 = aiPlayer
    win <- turn p1 p2 game True
    print win