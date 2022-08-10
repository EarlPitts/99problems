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

-- Problem 11
data Encoding a =
    Multiple Int a
  | Single a
  deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified x = (\x ->
      if length x == 1
      then Single (head x)
      else Multiple (length x) (head x))
    <$> pack x

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified = concatMap decodeSingle
  where decodeSingle (Multiple n c) = replicate n c
        decodeSingle (Single c) = [c]

-- Problem 13 TODO

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = rep n x <> repli xs n
  where rep 0 x = []
        rep n x = x : rep (n-1) x

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = go l n
  where go [] _ = []
        go (x:xs) 1 = go xs n
        go (x:xs) n = x : go xs (n - 1)
        
-- Problem 17
split :: [a] -> Int -> ([a],[a])
split l n = (\(x,y) -> (reverse x, y)) $ go [] l n
  where go acc (x:xs) 1 = (x:acc,xs)
        go acc (x:xs) n = go (x:acc) xs (n-1)
        go acc [] _ = (acc,[])
        
-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) _ 1 = [x]
slice (x:xs) 1 e = x : slice xs 1 (e-1)
slice (x:xs) s e = slice xs (s-1) (e-1)

-- Problem 19 TODO
rotate :: [a] -> Int -> [a]
rotate = undefined

-- Problem 20 TODO
removeAt :: Int -> [a] -> [a]
removeAt = go
  where go 1 (x:xs) = go 0 xs
        go n (x:xs) = x : go (n-1) xs
        go _ [] = []
