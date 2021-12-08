module Utils where

import           Data.Vector ( Vector, fromList )
import qualified Data.Vector as V

fromRight' :: Either a b -> b
fromRight' (Right a) = a
fromRight' (Left  _) = error "parse error"

-- why isn't this in the prelude?
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)

erase :: Eq a => Vector a -> a -> Vector a
erase v a = V.takeWhile (/= a) v V.++ V.tail (V.dropWhile (/= a) v)

-- v[l .. l + length u] := u
replace :: Vector a -> Vector a -> Int -> Vector a
replace v u l = V.update_ v (fromList [l .. l + length u]) u

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = if f a == a then a else fixpoint f (f a)
