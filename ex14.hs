dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x,x]) xs

test1 :: Bool
test1 = dupli [1, 2, 3] == [1,1,2,2,3,3]

-- test2 :: Bool
-- test2 = dupli [] == []
