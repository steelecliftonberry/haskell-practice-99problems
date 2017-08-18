import Test.QuickCheck

insertAt :: a -> [a] -> Int -> [a]
insertAt el [] _ = [el]
insertAt el (x:xs) n
  | n <= 1 = (el:x:xs)
  | otherwise = x : insertAt el xs (n-1)

test1 :: Bool
test1 = insertAt 'X' "abcd" 2 == "aXbcd"

property_index el list n
  | n >= length list = last (insertAt el list n) == el
  | n >= 1 = (insertAt el list n) !! (n-1) == el
  | otherwise = head (insertAt el list n) == el
  where types = el::Char
