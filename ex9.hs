pack :: Eq a => [a] -> [[a]]
pack (x:xs) = pack' (xs) [x]

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] remainder = [remainder]
pack' (x:xs) (y:ys)
  | x == y = pack' xs (x:y:ys)
  | otherwise = (y:ys) : pack' xs [x]

test1 :: Bool
test1 = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa","b","cc","aa","d","eeee"]
