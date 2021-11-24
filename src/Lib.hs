module Lib
    ( someFunc
    ) where

import Data.Vector

v :: Vector Int
v = singleton 3

someFunc :: IO ()
someFunc = print $ v ! 0
