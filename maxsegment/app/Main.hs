module Main where

import Lib

import Data.List
import Data.Monoid

-- nums :: [Int]
nums = [31, -41, 59, 26, -53, 58, 97, -93, -23, 84]

inits' = tail . inits
tails' = init . tails
segs = concat . map tails' . inits'

sol1 = maximum . map sum . segs

-- We replace segs with it's definition
sol2 = maximum . map sum . concat . map tails' . inits'

-- Push sum under concat
-- map sum . concat = concat . map (map sum)
sol3 = maximum . concat . map (map sum) . map tails' . inits'

-- maximum . concat = maximum . map maximum
sol4 = maximum . map maximum . map (map sum) . map tails' . inits'

-- map (map sum) . map tails' = map (map sum . tails')
-- map f . map g = map (f . g)
sol5 = maximum . map maximum . map (map sum . tails') . inits'

-- map maximum . map (map sum . tails') = map (maximum . (map sum . tails'))
-- map f . map g = map (f . g)
sol6 = maximum . map (maximum . map sum . tails') . inits'

d = -1000 -- minBound -- -Infinity
e = 0

sol7 = maximum . map ((foldl (\x y -> if x > y then x else y) d) . map (foldl (\x y -> x + y) e) . tails') . inits'
sol8 = maximum . map ((foldl max d) . map (foldl (+) e) . tails') . inits'
--                         S        . map      T        . tails'
-- S = Max, max,  d = -Infinity
-- T = Sum, plus, d = 0

-- o x y = (max x d) + y
-- sol9 = maximum . map (foldr o d . tails') . inits'
final = maximum . scanr h 0
  where h x y = 0 `max` (x + y)

sol9 = maximum . map (maximum . map sum . inits') . tails'

-- map sum . inits' = scanl (+) 0
sol10 = maximum . map (maximum . scanl (+) 0) . tails'

-- maximum = foldr1 max
sol11 = foldr1 max . map (foldr1 max . scanl (+) 0) .tails'


main :: IO ()
main = someFunc
