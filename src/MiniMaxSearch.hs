module MiniMaxSearch where

import Data.List ((\\), nub, groupBy, sortBy, subsequences, sort)
import Data.Function (on)
import Data.Maybe (fromJust)

import GameData
import MiniMaxGeneric 


-- Constructs a minimax search node at the endgame when the opponent's hand is known.
-- NB: the second boolean parameter (attack/defense) must be changed accordingly when
-- passing the node to the algorithm.
constructSN :: PlayerVisibleState -> Maybe SearchNode
constructSN pvs@(PlayerVisibleState myHand _ _ trump koHand _ 0 ts) =
    Just $ SearchNode myHand opponentHand trump True False ts
    where opponentHand = unseenCards pvs ++ koHand 
constructSN _ = Nothing

-- Minimax search node. Similar to the GameState but without unneeded information.
data SearchNode = SearchNode { p1Hand    :: [Card]
                             , p2Hand    :: [Card]
                             , trump     :: Card
                             , isPlayer1 :: Bool --Whose turn is it now?
                             , isAttack  :: Bool --Do they need to attack?
                             , transient :: TransientState }
                             deriving (Eq, Ord, Show)

myHand :: SearchNode -> [Card]
myHand (SearchNode h _ _ True _ _) = h
myHand (SearchNode _ h _ False _ _) = h

oppHand :: SearchNode -> [Card]
oppHand (SearchNode h _ _ False _ _) = h
oppHand (SearchNode _ h _ True _ _) = h

--Same as in GameEngine, see that for documentation
generateAttacks :: SearchNode -> [OffenseAction]
generateAttacks sn@(SearchNode _ _ _ _ _ ts)
    | null $ oppHand sn      = [FinishAttack]
    | null $ allDeskCards ts = map Attack  . filter ((>=) (length $ oppHand sn) . length) . filter (not . null) . concatMap subsequences $ 
        groupBy ((==) `on` cardValue) $ sortBy (compare `on` cardValue) hand -- group into equivalence classes on card values 
    | otherwise              = FinishAttack : (map Attack . filter (not . null) . subsequences $ filter (flip elem attackValues . cardValue) hand)  
    where attackValues = map cardValue $ allDeskCards ts
          hand         = myHand sn

generateDefenses :: SearchNode -> [DefenseAction]
generateDefenses sn@(SearchNode _ _ tr _ _ ts) = 
    GiveUp : map (Defend . zip (activeAttack ts)) validBeatings
    where cardsBeatC ac = filter (\pc -> beats (cardSuit tr) pc ac) $ myHand sn 
          beatingCards  = map cardsBeatC $ activeAttack ts 
          validBeatings = filter (\xs -> nub xs == xs) $ sequence beatingCards

applyAttack :: SearchNode -> OffenseAction -> SearchNode
applyAttack sn FinishAttack =
    sn {transient = emptyTransientState, isPlayer1 = not (isPlayer1 sn), isAttack = True}
applyAttack sn@(SearchNode p1h _ _ True _ ts) (Attack cards) =
    sn {p1Hand = p1h \\ cards, isPlayer1 = False, isAttack = False, transient = ts { activeAttack = activeAttack ts ++ cards}}
applyAttack sn@(SearchNode _ p2h _ False _ ts) (Attack cards) =
    sn {p2Hand = p2h \\ cards, isPlayer1 = True, isAttack = False, transient = ts { activeAttack = activeAttack ts ++ cards}}
     
applyDefense :: SearchNode -> DefenseAction -> SearchNode
applyDefense sn@(SearchNode p1h _ _ True _ ts@(TransientState ia ind aa)) (Defend cards) =
    sn {p1Hand = p1h \\ with, isPlayer1 = False, isAttack = True,
        transient = ts {inactiveAttack  = against ++ ia,
                       activeAttack    = aa \\ against,
                       inactiveDefense = ind ++ with}}
    where against = map fst cards
          with    = map snd cards
applyDefense sn@(SearchNode _ p2h _ False _ ts@(TransientState ia ind aa)) (Defend cards) =
    sn {p2Hand = p2h \\ with, isPlayer1 = True, isAttack = True,
        transient = ts {inactiveAttack  = against ++ ia,
                       activeAttack    = aa \\ against,
                       inactiveDefense = ind ++ with}}
    where against = map fst cards
          with    = map snd cards
 
applyDefense sn@(SearchNode p1h _ _ True _ ts) GiveUp =
    sn {p1Hand = p1h ++ allDeskCards ts, isPlayer1 = False, isAttack = True, transient = emptyTransientState} 
applyDefense sn@(SearchNode _ p2h _ False _ ts) GiveUp =
    sn {p2Hand = p2h ++ allDeskCards ts, isPlayer1 = True, isAttack = True, transient = emptyTransientState}

instance MiniMaxSearchNode SearchNode where
    getValue (SearchNode [] _ _ _ _ _) = Just 1
    getValue (SearchNode _ [] _ _ _ _) = Just (-1)
    getValue _                         = Nothing
    
    generateNodes sn@(SearchNode _ _ _ _ True _) =
        map (applyAttack sn) $ generateAttacks sn
    generateNodes sn@(SearchNode _ _ _ _ False _) =
        map (applyDefense sn) $ generateDefenses sn
    
    canonicalForm sn@(SearchNode p1h p2h _ _ _ ts@(TransientState ina ind aa)) =
        sn {p1Hand = sort p1h, p2Hand = sort p2h, 
            transient = ts {inactiveAttack  = sort ina,
                            inactiveDefense = sort ind,
                            activeAttack    = sort aa}}                                     
    
    -- If it's P2's turn now, P1 has just performed his turn
    -- and so we need to maximize the value (since we're 
    -- evaluating the best state and hence the best action for P1) 
    isMaximize = not . isPlayer1

-- Determine the best defense/attack player 1 can perform from a certain search node.
minmaxDefense :: SearchNode -> [DefenseAction] -> DefenseAction
minmaxDefense node actions = 
    fromJust $ lookup bestNode $ zip results actions 
    where results = map (applyDefense node{isAttack = False}) actions
          bestNode = getBestNode results

minmaxAttack :: SearchNode -> [OffenseAction] -> OffenseAction
minmaxAttack node actions = 
    fromJust $ lookup bestNode $ zip results actions 
    where results = map (applyAttack node{isAttack = True}) actions
          bestNode = getBestNode results
