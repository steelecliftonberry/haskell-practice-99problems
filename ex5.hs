myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

test1 :: Bool
test1 = myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"

test2 :: Bool
test2 = myReverse [1,2,3,4] == [4,3,2,1]

-- Failure tests

test3 :: Bool
test3 = myReverse "A man, a plan, a canal, panama!" == "A man, a plan, a canal, panama!"

test4 :: Bool
test4 = myReverse [1,2,3,4] == [1,2,3,4]
