module StingyJoe (
        solveSimple,
        solveCosts
        ) where
            
import Data.Array
import Data.List

solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (nodes, list) = let 
            --inf value:
                inf = 999999
            --ShortestPath
                shortestPath = let    
                --Helpers                    
                --get the length from the tuple
                    getLength (_,  _, d) = d
                    
                    rBounds = ((1, 1), (nodes, nodes)) 
                    
                --Adjacency matrix 
                    roadLength = listArray rBounds [myRoad i j| (i, j) <- range rBounds]
                    
                --If there is no road between those cities ->infinite 
                --otherwise -> the length of the road from the input list
                    myRoad i j = let
                                            rList i j = filter (\(a, b, c) -> if (a == i && b == j) then True else False) list
                                            in if length (rList i j) == 0 then inf else ( getLength $ head (rList i j) )
                                                
                --The list of visited cities
                    insertNode a b = (fst a) `union` (fst b)
                    
                --Add the distance between two cities
                    addEdges a b = (snd a) + (snd b)
                    
                --Minumum between two entities
                    myMin a b 
                        |(snd a) < (snd b) = a
                        |otherwise = b
                   
                --Obtain the desired result using a slightly modified version of Floyd-Warshall algorithm
                    bounds = ((1, 1, 1), (nodes, nodes, nodes))
                    matrix = listArray bounds [ cost i j k | (i,  j, k) <- range bounds]
                    
                    cost i j k 
                        | i == j  && k == 1 = ([i], 0) 
                        | (roadLength ! (i, j) ) /= inf && k == 1 = ([i], (roadLength ! (i, j) ))
                        | k == 1 =([inf], inf)
                        | otherwise = myMin
                                            (matrix ! (i, j, (k - 1)))
                                            ( 
                                                insertNode (matrix ! (i, k, (k - 1))) (matrix ! (k, j, (k - 1) )),
                                                addEdges (matrix ! (i, k, (k - 1) ))  (matrix ! (k, j, (k - 1) ))
                                            )
                    in matrix ! (1, nodes, nodes) 
                in if (snd shortestPath == inf) then Nothing else (Just shortestPath)

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (nodes, money, entranceFees, list) = let 
                --infinite value:
                    inf = 999999
                
                --The entrance fee for each city
                    fees = listArray (1, nodes) entranceFees
                
                --ShortestPath
                    shortestPath = let    
                    --Helpers                    
                    --Get the length from the touple
                        getLength (_,  _, d) = d
                        
                        rBounds = ((1, 1), (nodes, nodes)) 
                        
                    --Adjacency matrix 
                        roadLength = listArray rBounds [myRoad i j| (i, j) <- range rBounds]
                        
                    --If there is no road between those cities ->infinite 
                    --otherwise -> the length of the road from the input list
                        myRoad i j = let
                                                rList i j = filter (\(a, b, c) -> if (a == i && b == j)  then True else False) list
                                                in if length (rList i j) == 0 then inf else ( getLength $ head (rList i j) )
                                                
                    --The list of visited cities
                        addNode a b = (fst a) `union` (fst b)
                        
                    --Add the distance between two cities
                        addEdges a b = (fst $ snd a) + (fst $ snd b)
                        
                    --The money spent to travel this far
                        addMoney a b =  (snd $ snd a) + sum ( map (\x-> if x <= nodes then fees !x else inf ) ( tail $ fst b) )
                        
                    --Minumum between two entities
                        myMinim a b 
                            |(fst $ snd a)  >  (fst $ snd b) && (snd $ snd b) <= money =b
                            |(fst $ snd a) == (fst $ snd b) && (snd $ snd a) > (snd $ snd b) =b
                            |otherwise =a
                        
                    --Obtain the desired result using a slightly modified version of Floyd-Warshall algorithm
                        bounds = ((1, 1, 1), (nodes, nodes, nodes))
                        matrix = listArray bounds [ cost i j k | (i,  j, k) <- range bounds]
                        
                        cost i j k 
                            | i == j  && k == 1 = ([i], (0, 0)) 
                            | (roadLength ! (i, j) ) /= inf  &&  k == 1 = ( i:[j], ((roadLength ! (i, j) ), fees ! j))
                            | k == 1 =( [inf], (inf, inf))
                            | otherwise = myMinim
                                                (matrix ! (i, j, (k - 1)))
                                                ( 
                                                    addNode (matrix ! (i, k, (k - 1)))  (matrix ! (k, j, (k - 1) )),
                                                    (addEdges (matrix ! (i, k, (k - 1) ))  (matrix ! (k, j, (k - 1) )),
                                                     addMoney  (matrix ! (i, k, (k - 1)))  (matrix ! (k, j, (k - 1) )))
                                                )
                        in matrix ! (1, nodes, nodes) 
                            
                    finalResult = let
                        --Builds a list whith the money left after visiting each city 
                            spentMoney cities = let 
                                                                leftMoney x [] = money : []
                                                                leftMoney x acc = acc ++ ( (last acc) - (fees ! x) ) : []  
                                                                in foldr leftMoney [] (reverse cities)
                                                                
                        --Builds a list of pairs " (city, money left after visiting that city) "
                            myActivity cities = zip cities (spentMoney cities) 
                            
                        --If there is a road between the first and the last city 
                        --      and you have enough money to go that way, the details will be shown 
                        in  if ((fst $ snd  shortestPath) /= inf && (snd $ snd shortestPath ) <= money) 
                            then Just ((myActivity (fst shortestPath)), (fst $ snd shortestPath)) 
                            else  Nothing
                    in finalResult
