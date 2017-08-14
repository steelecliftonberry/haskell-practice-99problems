-- Take aways from the first 20 exercises:
-- 1) Some equal solutions have wildly different space-time complexity requirements hidden behind higher-order functions or otherwise.
-- 2) I should use QuickCheck for stronger confidence.
-- 3) I should look at standard ways of error-handling and dealing with bad paramters, e.g. the Maybe monad.

removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ (drop n xs))

test1 :: Bool
test1 = removeAt 2 "abcd" == ('b',"acd")
