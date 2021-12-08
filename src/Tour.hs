module Tour where

import           Data.Vector ( Vector, singleton )
import qualified Data.Vector as V

import Node ( Node, distance )

newtype Tour = Tour (Vector Node)
  deriving Eq

-- | first element -> sequence of nodes in the solution,
--   second element -> remaining nodes to be added
type PartialSolution = (Tour, Tour)

evalT :: Tour -> Double
evalT (Tour ns) = case V.uncons ns of
  Nothing -> error "trying to eval the empty list"
  Just (h, t) -> let pairs = V.zip ns (t V.++ singleton h)
                 in  V.foldr (\(a, b) acc -> distance a b + acc) 0 pairs

-- calculate the cost of the solution
eval :: PartialSolution -> Double
eval (ns, _) = evalT ns

instance Ord Tour where
  (<) t1 t2 = evalT t1 < evalT t2
  (<=) t1 t2 = evalT t1 <= evalT t2
