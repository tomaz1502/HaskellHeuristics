{-# LANGUAGE RecordWildCards #-}

module TSPInstance where

import           Node

import           Text.Parsec
import           Text.Parsec.String

import           Control.Monad
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)

-- | information about the instance of the problem. nodes is the position in the
--   2-d plane of each node
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

parseTSPInstance :: Parser TSPInstance
parseTSPInstance = do skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      nm <- many (noneOf "\n")
                      jumpTwoLines
                      skipMany (noneOf " ")
                      skipMany (oneOf " ")
                      numNodes <- toInt <$> many (noneOf "\n")
                      jumpTwoLines
                      nodes <- replicateM numNodes parseNode
                      case nodes of
                        [] -> error "empty list of nodes is not allowed"
                        (h : t) -> return $ TSPInstance numNodes nm (h NE.:| t)
  where toInt s = read s :: Int
        skipLine = skipMany (noneOf "\n") >> skipMany (oneOf "\n")
        jumpTwoLines = skipMany (oneOf "\n") >> skipLine >> skipLine


-- | first element -> sequence of nodes in the solution,
--   second element -> remaining nodes to be added
type PartialSolution = ([Node], [Node])

-- calculate the cost of the solution
eval :: PartialSolution -> Double
eval ([], _)         = error "trying to eval the empty list"
eval (ns@(h : t), _) = let pairs = zip ns (t ++ [h])
                       in  foldr (\(a, b) acc -> distance a b + acc) 0 pairs
