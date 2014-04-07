module MiniMaxGeneric where

import qualified Data.Map as M

class Ord m => MiniMaxSearchNode m where
    getValue      :: m -> Maybe Int
    generateNodes :: m -> [m]
    canonicalForm :: m -> m
    isMaximize    :: m -> Bool

memberC ::  MiniMaxSearchNode m => m -> Cache m -> Bool
memberC n (Cache c) = M.member (canonicalForm n) c
lookupC :: MiniMaxSearchNode m => m -> Cache m -> Int
lookupC n (Cache c) = c M.! canonicalForm n
insertC :: MiniMaxSearchNode m => m -> Int -> Cache m -> Cache m
insertC n i (Cache c) = Cache $ M.insert (canonicalForm n) i c

newtype Cache m = Cache (M.Map m Int)

-- Folds the cached evaluateNode over several SearchNodes, threading the cache through the computation.
-- Returns the final cache, the best value and the node with the best value.
-- TODO: make into a monad?
cachedExtremum :: MiniMaxSearchNode m => Cache m -> [m] -> (Cache m, Int, m)
cachedExtremum cache (startNode:nodes) =
    foldl fn (startCache, startVal, startNode) nodes
    where (startCache, startVal) = cachedEvaluateNode cache startNode
          fn (currCache, currVal, currNode) newNode =
              let op                 = if isMaximize currNode then (>) else (<)
                  (newCache, newVal) = cachedEvaluateNode currCache newNode in 
              if op newVal currVal then (newCache, newVal, newNode)
                                   else (newCache, currVal, currNode)
                                        
              
-- Evaluates a search state's score using a minimax search.
cachedEvaluateNode :: MiniMaxSearchNode m => Cache m -> m -> (Cache m, Int)
cachedEvaluateNode cache sn = case getValue sn of
    Just value -> (cache, value)
    Nothing ->    if memberC sn cache then (cache, lookupC sn cache)
        else (\(c, i, _) -> (insertC sn i c,i)) . cachedExtremum cache $ generateNodes sn 

getBestNode :: MiniMaxSearchNode m => [m] -> m
getBestNode = (\(_, _, n) -> n) . cachedExtremum (Cache M.empty)

