module Utils where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' (Left  _) = error "parse error"

-- why isn't this in the prelude?
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)

maybeNEToList :: Maybe (NonEmpty a) -> [a]
maybeNEToList Nothing   = []
maybeNEToList (Just xs) = NE.toList xs
