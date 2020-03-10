-- Inf2d Assignment 1 2019-2020
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy, elemIndices, elemIndex)
import ConnectFourWithTwist


{- NOTES:
-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.
-- Comment your code.
-- You should submit this file when you have finished the assignment.
-- The deadline is the  10th March 2020 at 3pm.
-- See the assignment sheet and document files for more information on the predefined game functions.
-- See the README for description of a user interface to test your code.
-- See www.haskell.org for haskell revision.
-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...
-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.
-}

-- Section 1: Uniform Search

-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph= [Node]

-- numNodes::Int
-- numNodes = 13


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

get::Node -> Int -> Graph -> [Node]
get node x g = if (node + x >= (((ceiling . sqrt . fromIntegral . length $ g)*(ceiling . sqrt . fromIntegral . length $ g)) - 1)) then []
               else if (g !! (node + x)) > 0 then x : get node (x+1) g 
               else if (x /= (ceiling . sqrt . fromIntegral . length $ g)) then get node (x+1) g
               else []


append::Branch -> [Node] -> [Branch]
append [] _ = []
append _ [] = []
append xs (y:ys) = (y : xs) : (append xs ys)

next::Branch -> Graph ->  [Branch]
next [] g = []
next xs g = append xs (get (numNodes * (head xs)) 0 g) 
    where numNodes = (ceiling . sqrt . fromIntegral . length $ g)


-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


explored::Node-> [Node] ->Bool
explored point [] = False
explored point (x:exploredList) = if (point == x) then True
                                  else explored point exploredList

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.


breadthFirstSearch::Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch [] _ _ _ _ = Nothing
breadthFirstSearch g destination next [] [] = breadthFirstSearch g destination next [[0]] []
breadthFirstSearch g destination next [] _ = Nothing
breadthFirstSearch g destination next branches exploredList = 
                                    let y = head branches
                                        x = head y                                                                
                                        bool = checkArrival x destination
                                        ans = explored x exploredList         
                                    in if (ans == False) then
                                            if (bool == True) then Just y
                                            else breadthFirstSearch g destination next (branches ++ (next y g)) (x:exploredList)
                                       else breadthFirstSearch g destination next (tail branches) exploredList

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.

depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch g destination next [] d [] = depthLimitedSearch g destination next [[0]] d []
depthLimitedSearch g destination next [] d exploredList = Nothing
depthLimitedSearch g destination next branches d exploredList = 
                                    let y = head branches
                                        x = head y
                                        bool = checkArrival x destination
                                        ans = explored x exploredList
                                    in if (bool == True && length y <= (d+1)) then Just y
                                       else if (length y > d) then depthLimitedSearch g destination next (tail branches) d (take (length (head(tail branches))) exploredList)
                                       else if (ans == True) then depthLimitedSearch g destination next (tail branches) d exploredList
                                       else depthLimitedSearch g destination next ((next y g) ++ branches) d (x:exploredList)
                                                         
-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph ->Branch  -> Int
cost gr [] = 0
cost gr [x] = 0
cost gr (x:y:branch) = if (gr !! (y*numNodes+x) > 0) then gr !! (y*numNodes+x) + cost gr (y:branch)
                       else maxBound `div` 2
                       where numNodes = ceiling . sqrt . fromIntegral . length $ gr


    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr [] _ = 0
getHr hrTable node = hrTable !! node  


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch g destination next getHr hrTable cost [] [] = aStarSearch g destination next getHr hrTable cost [[0]] []
aStarSearch g destination next getHr hrTable cost branches exploredList = 
                                    let y = head branches
                                        x = head y
                                        bool = checkArrival x destination
                                        ans = explored x exploredList
                                    in if bool == True then Just y
                                       else if ans == True then aStarSearch g destination next getHr hrTable cost (tail sortedList) exploredList
                                       else aStarSearch g destination next getHr hrTable cost sortedList (x:exploredList)
                                    where sortedList = isort g hrTable (branches ++ (next (head branches) g))



-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game = if checkWin game maxPlayer then 1
            else if checkWin game minPlayer then (-1)
            else 0

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 
alphabeta:: Role -> Game -> Int
alphabeta player game | player == maxPlayer = maxValue game (-2) 2
                      | otherwise = minValue game (-2) 2

maxValue::Game ->Int ->Int ->Int
maxValue g a b | terminal g = eval g
               | otherwise = helperMaxFct (movesAndTurns g maxPlayer) (-2) a b (switch maxPlayer)


minValue::Game ->Int ->Int ->Int
minValue g a b | terminal g = eval g
               | otherwise = helperMinFct (movesAndTurns g minPlayer) 2 a b (switch minPlayer)


helperMaxFct::[Game] ->Int ->Int ->Int ->Role ->Int
helperMaxFct [] v a b r = v
helperMaxFct (g:games) v a b r | v >= b = v
                               | otherwise = helperMaxFct games (max v (maxValue g a b)) (max v a) b r
                          


helperMinFct::[Game] ->Int ->Int ->Int ->Role ->Int
helperMinFct [] v a b r = v
helperMinFct (g:games) v a b r | v <= a = v
                               | otherwise = helperMinFct games (min v (minValue g a b)) a (min b v) r
                          

-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player game=undefined
--{ Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores


--Auxiliary functions for A-Star Search

compute::Graph->Branch->[Int]->Int
compute g [] hrTable = 0   
compute g branch hrTable = (cost g branch) + (getHr hrTable (head branch))    --computing the total cost for a given branch


insert::Graph ->[Int] ->Branch ->[Branch] ->[Branch]     --helper function for insertion sort
insert g hrTable x [] = [x]
insert g hrTable x (y:ys) = if ((compute g x hrTable) <= (compute g y hrTable)) then x:y:ys
                            else y:(insert g hrTable x ys)

isort::Graph ->[Int] ->[Branch] ->[Branch]      --insertion sort
isort g hrTable [] = []
isort g hrTable (b:branches) = insert g hrTable b (isort g hrTable branches)

--}
