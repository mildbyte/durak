module Main where

import System.Random (getStdGen)
import GameEngine
import HumanPlayer
import AI

main::IO()
main = do
    gen  <- getStdGen
    let game = generateGame gen
    let p1 = HumanPlayer
    let p2 = HumanPlayer
    let win = turn p1 p2 game True
    print win