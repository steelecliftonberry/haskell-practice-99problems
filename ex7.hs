--  Haskell lists have elements of homogenous type, so define new List type.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

test1 :: Bool
test1 = flatten (Elem 5) == [5]

test2 :: Bool
test2 = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]

-- Commented out because compiler complains about Equality ambiguity, but confirmed true in REPL
-- test3 :: Bool
-- test3 = flatten (List []) == []
