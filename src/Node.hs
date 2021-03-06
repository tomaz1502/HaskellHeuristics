{-# LANGUAGE RecordWildCards #-}

module Node where

import Text.Parsec ( noneOf, space, many1, many, skipMany )
import Text.Parsec.String ( Parser )

-- | A point in the 2-d plane
data Node =
  Node { xc :: Double
       , yc :: Double
       }

instance Show Node where
  show Node {..} = unwords ["(", show xc, ",", show yc, ")"]

instance Eq Node where
  n1 == n2 = abs (xc n1 - xc n2) < eps &&
             abs (yc n1 - yc n2) < eps
    where eps = 10 ^ (-9)

instance Ord Node where
  n1 < n2
    | xc n1 /= xc n2  = xc n1 < xc n2
    | otherwise       = yc n1 < yc n2
  n1 <= n2 = n1 < n2 || n1 == n2

distance :: Node -> Node -> Double
distance Node {xc = x1, yc = y1} Node {xc = x2, yc = y2} =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

parseNode :: Parser Node
parseNode = do skipMany space
               skipMany (noneOf " ")
               skipMany space
               xc <- toDouble <$> many (noneOf " ")
               skipMany space
               yc <- toDouble <$> many1 (noneOf "\n")
               return $ Node xc yc
  where toDouble s = read s :: Double
