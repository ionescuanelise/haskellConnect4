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

numNodes::Int
numNodes = 5


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.


get::Node -> Int -> Graph -> [Node]
get node x g = if (node + x >= ((numNodes*numNodes) - 1)) then []
               else if (g !! (node + x)) > 0 then x : get node (x+1) g 
               else if (x /= numNodes) then get node (x+1) g
               else []


append::Branch -> [Node] -> [Branch]
append [] _ = []
append _ [] = []
append xs (y:ys) = (y : xs) : (append xs ys)

next::Branch -> Graph ->  [Branch]
next [] g = []
next xs g = append xs (get (numNodes * (head xs)) 0 g) 



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
breadthFirstSearch g destination next [] [] = breadthFirstSearch g destination next [[0]] []
breadthFirstSearch g destination next [] exploredList = Nothing
breadthFirstSearch g destination next branches exploredList = 
                                    let y = head branches
                                        x = head y                                                                
                                        bool = checkArrival x destination
                                        ans = explored x exploredList
                                    in if bool == True then Just y
                                       else if ans == False then breadthFirstSearch g destination next (branches ++ (next y g)) (x:exploredList)
                                       else breadthFirstSearch g destination next (tail branches) exploredList          


-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch
depthLimitedSearch g destination next branches  d exploredList = undefined

depthFirstSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int-> [Node]-> Maybe Branch
depthFirstSearch g destination next [] d [] = depthFirstSearch g destination next [[0]] d []
depthFirstSearch g destination next [] d exploredList = Nothing
depthFirstSearch g destination next branches d exploredList = 
                                    let y = head branches
                                        x = head y
                                        bool = checkArrival x destination
                                        ans = explored x exploredList
                                    in if bool == True then Just y
                                       else if (length y > d || ans == True) then depthFirstSearch g destination next (tail branches) d exploredList
                                       else depthFirstSearch g destination next ((next y g) ++ branches) d (x:exploredList)
                                       
                                       
-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph ->Branch  -> Int
cost gr [] = 0
cost gr [x] = 0
cost gr (x:y:branch) = (gr !! (numNodes*y+x)) + cost gr (y:branch) 


    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr hrTable node = hrTable !! node  


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch g destination next getHr hrTable cost branches exploredList =undefined

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game = undefined

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 
alphabeta:: Role -> Game -> Int
alphabeta  player game = undefined


-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player game=undefined
{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}