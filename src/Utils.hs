module Utils where

import           Data.Vector ( Vector )
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
