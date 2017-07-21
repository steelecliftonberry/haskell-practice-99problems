pack :: Eq a => [a] -> [[a]]
pack (x:xs) = pack' (xs) [x]

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] remainder = [remainder]
pack' (x:xs) (y:ys)
  | x == y = pack' xs (x:y:ys)
  | otherwise = (y:ys) : pack' xs [x]

compress :: Eq a => [[a]] -> [(Int,a)]
compress [] = []
compress (x:xs) = (length x, head x) : compress xs

encode :: Eq a => [a] -> [(Int,a)]
encode list = compress (pack list)

test1 :: Bool
test1 = encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
