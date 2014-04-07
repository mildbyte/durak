module HumanPlayer where
import GameData
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)

humanPlayer :: Player
humanPlayer = Player inputAction inputAction outputDefense outputOffense

outputOffense :: OffenseAction -> IO()
outputOffense FinishAttack = putStrLn "The opponent has finished the attack!"
outputOffense (Attack cs)  = do 
    putStrLn "The opponent attacks!"
    forM_ cs (\c -> putStrLn $ " * " ++ show c)
    
outputDefense :: DefenseAction -> IO()
outputDefense GiveUp      = putStrLn "The opponent gives up!"
outputDefense (Defend cs) = do
    putStrLn "The opponent: "
    forM_ cs (\(w, a) -> putStrLn $ " * Beats " ++ show w ++ " with " ++ show a)

verifiedInput :: Int -> Int -> IO Int
verifiedInput minN maxN = do
    choice <- getLine
    case reads choice of
        [(c, "")] | c >= minN && c < maxN -> return c
        _         -> do putStrLn "Wrong choice, try again"
                        verifiedInput minN maxN
    
inputAction :: Show b => PlayerVisibleState -> [b] -> IO b
inputAction (PlayerVisibleState hand tHand _ trump _ _ remD (TransientState ina ind aa)) actions = do
    putStrLn $ "Your hand: " ++ show hand ++ " (from which you had to take " ++ show tHand ++ ")"
    putStrLn $ "Trump card:" ++ show trump
    putStrLn $ "Cards on desk: " ++ show (ina ++ ind)
    putStrLn $ "Cards to be beaten: " ++ show aa
    putStrLn $ "Cards still in the deck: " ++ show remD
    
    forM_ (zip [1..] actions) (\(n, a) -> putStrLn $ show n ++ ") " ++ show a)
    choice <- verifiedInput 1 (length actions + 1)
    return $ actions !! (choice - 1)