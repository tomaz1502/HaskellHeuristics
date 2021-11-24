{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Text.Parsec
import           Text.Parsec.String

import           Data.List          (delete)
import           Control.Monad
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | A point in a 2-D plane
data Node =
  Node { xc :: Double
       , yc :: Double
       }

instance Show Node where
  show Node {..} = unwords ["(", show xc, ",", show yc, ")"]

instance Eq Node where
  n1 == n2 = xc n1 == xc n2 &&
             yc n1 == yc n2

distance :: Node -> Node -> Double
distance Node {xc = x1, yc = y1} Node {xc = x2, yc = y2} =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

data TSPInstance =
  TSPInstance { numNodes :: Int
              , instName :: String
              , nodes    :: NonEmpty Node
              }

instance Show TSPInstance where
  show TSPInstance {..} = unlines [ "name: " ++ instName
                                  , "numNodes: " ++ show numNodes
                                  , "nodes: " ++ show nodes
                                  ]

parseNode :: Parser Node
parseNode = do skipMany (noneOf " ")
               skipMany (oneOf " ")
               xc <- toDouble <$> many (noneOf " ")
               skipMany (oneOf " ")
               yc <- toDouble <$> many1 (noneOf " \n")
               return $ Node xc yc
  where toDouble s = read s :: Double

parseTSPInstance :: Parser TSPInstance
parseTSPInstance = do skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      nm <- many (noneOf "\n")
                      skipMany (oneOf "\n")
                      skipLine
                      skipLine
                      skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      numNodes <- toInt <$> many (noneOf "\n")
                      skipMany (oneOf "\n")
                      skipLine
                      skipLine
                      nodes <- replicateM numNodes parseNode
                      case nodes of
                        [] -> error "empty list of nodes is not allowed"
                        (h : t) -> return $ TSPInstance numNodes nm (h NE.:| t)
  where toInt s = read s :: Int
        skipLine = skipMany (noneOf "\n") >> skipMany (oneOf "\n")


type PartialSolution = ([Node], [Node])

eval :: PartialSolution -> Double
eval ([], _)    = error "trying to eval the empty list"
eval (ns@(h : t), _) = let pairs = zip ns (t ++ [h])
                       in  foldr (\(a, b) acc -> distance a b + acc) 0 pairs

data ConsHeur =
  ConsHeur { initSol :: TSPInstance -> PartialSolution
           , step :: PartialSolution -> PartialSolution
           }

solve :: ConsHeur -> TSPInstance -> Double
solve ConsHeur {..} ti = eval $ applyN (numNodes ti - length stNodes) step st
  where st@(stNodes, _) = initSol ti

initNearNeigh :: TSPInstance -> PartialSolution
initNearNeigh ti = let (h, mt) = NE.uncons (nodes ti)
                   in ([h], maybeNEToList mt)

stepNearNeigh :: PartialSolution -> PartialSolution
stepNearNeigh sol@(_, []) = sol
stepNearNeigh (path, rem@(c1 : cs)) =
        let border = last path
            chosen = foldr (\n1 n2 -> if distance border n1 < distance border n2
                                      then n1 else n2) c1 cs
        in (path ++ [chosen], delete chosen rem)

nearestNeighbour :: ConsHeur
nearestNeighbour = ConsHeur initNearNeigh stepNearNeigh

-- utils
-- sadly, doesn't exist in the prelude :(
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)

maybeNEToList :: Maybe (NonEmpty a) -> [a]
maybeNEToList Nothing   = []
maybeNEToList (Just xs) = NE.toList xs
