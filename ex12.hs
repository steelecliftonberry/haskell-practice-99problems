data EncodeUnit a = Single a | Multiple Int a
  deriving (Eq, Show)

decodeModified :: [EncodeUnit a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) = (replicate n x) ++ decodeModified xs

test1 :: Bool
test1 = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee"
