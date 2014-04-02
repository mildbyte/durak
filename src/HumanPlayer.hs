module HumanPlayer where
import GameData
import Control.Monad (forM_)

data HumanPlayer = HumanPlayer

instance Player HumanPlayer where
    getDefenseAction _ = inputAction
    getOffenseAction _ = inputAction

inputAction pvs@(PlayerVisibleState hand _ trump _ _ _ transientState) actions = do
    print pvs
    forM_ (zip actions [1..]) print
    choice <- readLn
    return $ actions !! (choice - 1)