import Test.QuickCheck

range :: Integer -> Integer -> [Integer]
range n m
  | n < m = n : range (succ n) m
  | n == m = [n]
  | n > m = n : range (pred n) m

test1 :: Bool
test1 = range 4 9 == [4,5,6,7,8,9]

property_bounds n m = (head result == n) && (last result == m)
  where result = range n m
