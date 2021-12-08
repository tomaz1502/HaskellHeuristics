module VND where

import Debug.Trace

import Neighborhood ( Neighborhood, getLocalBest )
import Tour ( Tour, evalT )

vnd :: [Neighborhood] -> Tour -> Tour
vnd n' = go n'
  where
    go :: [Neighborhood] -> Tour -> Tour
    go [] t = t
    go (n : ns) t =
      let b = getLocalBest n t
      in if b >= t then go ns t else go n' b
