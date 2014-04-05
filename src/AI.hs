module AI where

import Data.List ((\\), maximumBy, nub, groupBy, sortBy, subsequences)
import Data.Function (on)
import qualified Data.Map as M

import GameData
import Data.Maybe (fromJust)

nothing :: a -> IO()
nothing _ = return()

aiPlayer :: Player
aiPlayer = Player daWrapper oaWrapper nothing nothing

oaWrapper gs actions = return $ chooseOffenseAction gs actions
daWrapper gs actions = return $ chooseDefenseAction gs actions

-- Constructs a minimax search node at the endgame when the opponent's hand is known.
-- NB: the second boolean parameter (attack/defense) must be changed accordingly when
-- passing the node to the algorithm.
constructSN :: PlayerVisibleState -> Maybe SearchNode
constructSN pvs@(PlayerVisibleState myHand _ _ trump koHand _ 0 ts) =
    Just $ SearchNode myHand opponentHand trump True False ts
    where opponentHand = unseenCards pvs ++ koHand 
constructSN _ = Nothing

-- Returns a list of cards that we haven't yet seen in the game
-- = universe of cards
-- - our hand - discard pile - face-up trump card - cards we know the opponent has - transient state
-- TODO: trump card sometimes is in the deck, sometimes not
unseenCards :: PlayerVisibleState -> [Card]
unseenCards (PlayerVisibleState p _ d t c _ _ ts) =
    universe \\ (t : p ++ d ++ c ++ allDeskCards ts)

-- Counts the expected value of the fraction of cards
-- the opponent has that satisfy a given predicate
-- Makes the estimate more precise by considering the cards
-- we know the opponent has.
cardFraction :: (Card -> Bool) -> PlayerVisibleState -> Double
cardFraction p
  state@(PlayerVisibleState _ _ _ _ knownCards opHandSize _ _)
  | knownLength == 0 = unknownEstimate
  | null possibleCards = knownFraction
  | otherwise =
    knownWeight * knownFraction + (1 - knownWeight) * unknownEstimate
  where knownWeight = knownLength / fromIntegral opHandSize
        knownFraction
          = fromIntegral (length $ filter p knownCards) / knownLength
        knownLength = fromIntegral $ length knownCards
        unknownEstimate
          = fromIntegral (length $ filter p possibleCards) /
              fromIntegral (length possibleCards)
        possibleCards = unseenCards state
        
-- The expected value of the fraction of cards
-- the opponent has that can't beat this card
offenseValue :: Card -> PlayerVisibleState -> Double
offenseValue card state =
    cardFraction (\c -> not (beats (cardSuit $ trumpCard state) c card)) state
    
-- The expected value of the fraction of cards
-- the opponent has that this card can beat
defenseValue :: Card -> PlayerVisibleState -> Double
defenseValue card state =
    cardFraction (beats (cardSuit $ trumpCard state) card) state

-- Weight for the hand size when evaluating a hand: less than 7 is okay,
-- apply a penalty after that.
-- TODO: small hands more valuable (after the deck runs out), but we would know the opponent's hand
-- by then; use minmax?
cardNumberMultiplier :: Int -> Double
cardNumberMultiplier n 
    | n == 6 = 1.0
    | n == 0 = 9000
    | n >  6 = 1.0 / (fromIntegral n - 6.0)
    | n <  6 = 1 + 1.0 / fromIntegral n

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Evaluates a hand's defense value given the current state.
-- Also takes the number of unknown cards in the hand (for cards we take from the deck)
handValue :: PlayerVisibleState -> [Card] -> Int -> Double
handValue gs cs unknown = mean (map (`defenseValue` gs) cs) * cardNumberMultiplier (length cs + unknown)

-- Evaluates a hand's defense value after we finish the turn and
-- take up some cards from the deck by taking a weighted average of
-- hand value and deck value.
-- If there are not enough cards in the deck, we will take the trump as well,
-- so the function adjusts our hand and the weight accordingly.
futureHandValue :: PlayerVisibleState -> [Card] -> Double
futureHandValue gs cs
    | length cs >= 6 = hVal  -- don't need to take cards from the deck
    | otherwise = hvWeight * hVal + (1 - hvWeight) * dVal
    where hvWeight = if lensum > 6
                     then fromIntegral (length cs) / 6.0
                     else fromIntegral (length cs + 1) / fromIntegral lensum
          hVal     = if lensum > 6
                     then handValue gs cs (6 - length cs)
                     else handValue gs (trumpCard gs : cs) (remainingDeckSize gs - 1)  -- will take all cards from the deck
          dVal     = deckValue gs                                                      -- but one of them is known
          lensum   = length cs + remainingDeckSize gs

-- Evaluates the average value of the cards in the deck
deckValue :: PlayerVisibleState -> Double
deckValue gs = mean $ map (`defenseValue` gs) $ unseenCards gs

-- Evaluates an offense action by considering our hand value after taking the cards from the deck.
evaluateOffenseAction :: PlayerVisibleState -> OffenseAction -> Double
evaluateOffenseAction gs FinishAttack = futureHandValue gs $ playerHand gs
evaluateOffenseAction gs (Attack cards) = futureHandValue gs $ playerHand gs \\ cards  

-- Evaluates a defense action by considering our hand value if we give up and take cards
-- or sacrifice our cards to defend against the attacking cards.
evaluateDefenseAction :: PlayerVisibleState -> DefenseAction -> Double
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ _ _ ts) GiveUp = futureHandValue gs (hand ++ allDeskCards ts)
evaluateDefenseAction gs@(PlayerVisibleState hand _ _ _ _ _ _ _) (Defend cards) = futureHandValue gs (hand \\ map snd cards) 

-- Minimax search node. Similar to the GameState but without unneeded information.
data SearchNode = SearchNode { p1Hand    :: [Card]
                             , p2Hand    :: [Card]
                             , trump     :: Card
                             , isPlayer1 :: Bool
                             , isAttack  :: Bool
                             , transient :: TransientState }
                             deriving (Eq, Ord)

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
applyDefense sn@(SearchNode p2h _ _ False _ ts@(TransientState ia ind aa)) (Defend cards) =
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
    
type Cache = M.Map SearchNode Int

-- Folds the cached evaluateNode over several SearchNodes, threading the cache through the computation.
-- Returns the final cache, the best value and the node with the best value.
-- op defines what we mean by best: (>) means greatest, (<) means smallest.
-- TODO: make into a monad?
cachedExtremum :: Cache -> (Int -> Int -> Bool) -> [SearchNode] -> (Cache, Int, SearchNode)
cachedExtremum cache op (startNode:nodes) =
    foldl fn (startCache, startVal, startNode) nodes
    where (startCache, startVal) = evaluateNode cache startNode
          fn (currCache, currVal, currNode) newNode =
              let (newCache, newVal) = evaluateNode currCache newNode in 
              if op newVal currVal then (newCache, newVal, newNode)
                                   else (newCache, currVal, currNode)
                                        
              
-- Evaluates a search state's score using a minimax search.
-- TODO: out of memory errors, investigate
evaluateNode :: Cache -> SearchNode -> (Cache, Int)
evaluateNode cache sn@(SearchNode p1h p2h _ isP1 isAtt _)
    | null p1h = (M.insert sn 1 cache, 1)
    | null p2h = (M.insert sn (-1) cache, -1)
    | otherwise = (\(c, i, _) -> (M.insert sn i c,i)) $ cachedExtremum cache (if isP1 then (>) else (<)) nextNodes 
        where nextNodes = if isAtt then map (applyAttack sn) $ generateAttacks sn
                                   else map (applyDefense sn) $ generateDefenses sn

-- Determine the best defense/attack player 1 can perform from a certain search node.
minmaxDefense :: SearchNode -> [DefenseAction] -> DefenseAction
minmaxDefense node actions = 
    fromJust $ lookup bestNode $ zip results actions 
    where results = map (applyDefense node{isAttack = False}) actions
          bestNode = (\(_, _, n) -> n) $ cachedExtremum M.empty (>) results

minmaxAttack :: SearchNode -> [OffenseAction] -> OffenseAction
minmaxAttack node actions = 
    fromJust $ lookup bestNode $ zip results actions 
    where results = map (applyAttack node{isAttack = True}) actions
          bestNode = (\(_, _, n) -> n) $ cachedExtremum M.empty (>) results


-- Choosing an action for now is just about finding one with the maximum value.
-- If, otherwise, we can reconstruct the whole game state, we are in the endgame and can use
-- minimax to determine what to do.
chooseDefenseAction :: PlayerVisibleState -> [DefenseAction] -> DefenseAction
chooseDefenseAction gs = case constructSN gs of 
    Nothing   -> maximumBy (compare `on` evaluateDefenseAction gs)
    Just node -> minmaxDefense node

chooseOffenseAction :: PlayerVisibleState -> [OffenseAction] -> OffenseAction
chooseOffenseAction gs = case constructSN gs of
    Nothing   -> maximumBy (compare `on` evaluateOffenseAction gs)
    Just node -> minmaxAttack node