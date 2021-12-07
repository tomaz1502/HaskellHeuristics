{-# LANGUAGE RecordWildCards #-}

module TSPInstance where

import           Node ( distance, Node, parseNode )

import           Data.Vector (Vector, fromList, singleton)
import qualified Data.Vector as V
import           Text.Parsec ( noneOf, oneOf, many, skipMany )
import           Text.Parsec.String ( Parser )

import           Control.Monad ( replicateM )

-- | information about the instance of the problem. nodes is the position in the
--   2-d plane of each node
data TSPInstance =
  TSPInstance { numNodes :: Int
              , instName :: String
              , nodes    :: Vector Node
              }

instance Show TSPInstance where
  show TSPInstance {..} = unlines [ "name: " ++ instName
                                  , "numNodes: " ++ show numNodes
                                  , "nodes: " ++ show nodes
                                  ]

parseTSPInstance :: Parser TSPInstance
parseTSPInstance = do skipMany (noneOf ":")
                      skipMany (oneOf ":")
                      nm <- many (noneOf "\n")
                      jumpTwoLines
                      skipMany (noneOf ":")
                      skipMany (oneOf ":")
                      numNodes <- toInt <$> many (noneOf "\n")
                      jumpTwoLines
                      nodes <- replicateM numNodes parseNode
                      case nodes of
                        [] -> error "empty list of nodes is not allowed"
                        ns@(h : t) -> return $ TSPInstance numNodes nm (fromList ns)
  where toInt s = read s :: Int
        skipLine = skipMany (noneOf "\n") >> skipMany (oneOf "\n")
        jumpTwoLines = skipMany (oneOf "\n") >> skipLine >> skipLine


-- | first element -> sequence of nodes in the solution,
--   second element -> remaining nodes to be added
type PartialSolution = (Vector Node, Vector Node)

-- calculate the cost of the solution
eval :: PartialSolution -> Double
eval (ns, _) = case V.uncons ns of
  Nothing -> error "trying to eval the empty list"
  Just (h, t) -> let pairs = V.zip ns (t V.++ singleton h)
                 in  V.foldr (\(a, b) acc -> distance a b + acc) 0 pairs
