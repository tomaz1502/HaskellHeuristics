{-# LANGUAGE RecordWildCards #-}

module Check where

import TSPInstance ( TSPInstance (..))
import Tour ( Tour (Tour) )

import Data.List.Unique ( allUnique )
import Data.Vector (toList)

-- | checks if given tour is a valid tour
-- a valid tour must pass exactly once on each city
-- TODO: check if the nodes in tour are indeed the same of TSPInstance
-- (not necessary for the current heuristics)
check :: Tour -> TSPInstance -> Bool
check (Tour ns) TSPInstance {..} =
  allUnique (toList ns) && length ns == numNodes
