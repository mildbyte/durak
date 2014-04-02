module HumanPlayer where
import GameData
import Control.Monad (forM_)

humanPlayer = Player inputAction inputAction

inputAction pvs@(PlayerVisibleState hand _ trump _ _ _ transientState) actions = do
    print pvs
    forM_ (zip actions [1..]) print
    choice <- readLn
    return $ actions !! (choice - 1)