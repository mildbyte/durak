module HumanPlayer where
import GameData
import Control.Monad (forM_)

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

inputAction :: Show b => PlayerVisibleState -> [b] -> IO b
inputAction (PlayerVisibleState hand tHand _ trump _ _ remD (TransientState ina ind aa)) actions = do
    putStrLn $ "Your hand: " ++ show hand ++ " (from which the opponent knows " ++ show tHand ++ ")"
    putStrLn $ "Trump card:" ++ show trump
    putStrLn $ "Cards on desk: " ++ show (ina ++ ind)
    putStrLn $ "Cards to be beaten: " ++ show aa
    putStrLn $ "Cards still in the deck: " ++ show remD
    
    forM_ (zip [1..] actions) (\(n, a) -> putStrLn $ show n ++ ") " ++ show a)
    choice <- readLn
    return $ actions !! (choice - 1)