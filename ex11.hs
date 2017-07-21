data EncodeUnit a = Single a | Multiple Int a
  deriving (Eq, Show)

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = pack' (xs) [x]

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] remainder = [remainder]
pack' (x:xs) (y:ys)
  | x == y = pack' xs (x:y:ys)
  | otherwise = (y:ys) : pack' xs [x]

compress :: Eq a => [[a]] -> [EncodeUnit a]
compress [] = []
compress (x:xs)
  | length x > 1 = Multiple (length x) (head x) : compress xs
  | otherwise = Single (head x) : compress xs

encodeModified :: Eq a => [a] -> [EncodeUnit a]
encodeModified xs = compress $ pack xs

test1 :: Bool
test1 = encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
