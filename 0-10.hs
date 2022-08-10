-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
myLast [] = error "Empty list"

-- Problem 2
myButLast :: [a] -> a
myButLast [y,_] = y
myButLast (x:xs) = myButLast xs
myButLast [] = error "Empty list"

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)
elementAt [] n = error "Index too large"

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Probem 4 (tail recursion)
myLength' :: [a] -> Int
myLength' l = go l 0
  where go [] cnt = cnt
        go (x:xs) cnt = go xs (cnt + 1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse l = go l []
  where go [] acc = acc
        go (x:xs) acc = go xs (x : acc)

-- Problem 5 (without tail recursion)
myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs <> [x]

-- Problem 6 (Easy solution)
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = myReverse' l == l

-- Problem 6 (Using stacks again)
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' l = go l []
  where go [] acc = acc == l
        go (x:xs) acc = go xs (x:acc)

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y
                    then compress (y:xs)
                    else x:compress (y:xs)


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack = go []
  where go acc [] = [acc]
        go acc [x] = go (x:acc) []
        go acc (x:y:xs) =
          if x == y
          then go (x:acc) (y:xs)
          else (x:acc) : go [] (y:xs)

-- Problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode x = (\x -> (length x, head x)) <$> pack x
