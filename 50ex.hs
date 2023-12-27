-- exe 1
myenumFromTo :: Int -> Int ->[Int]
myenumFromTo x y |x >= y = x : []
                 |x < y = x: myenumFromto (x+1) y
                 |otherwise = []

-- exe 2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z |x >= z = x : []
                       |x < z = x : myenumFromThenTo y (y + (y-x)) z
                       |otherwise = []

--exe 3
(+++) :: [a] -> [a] -> [a]
(+++) l [] = l
(+++) [] l = l
(+++) (x:xs) l = x : ((+++) xs l)

--exe 4
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) t (n-1)

--exe 5
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs + [x]

--exe 6
take :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs) = if n > 0 then x : take (n-1) xs else []

--exe 7
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 l = l
drop n (x:xs) = if n > 0 then drop (n-1) xs else (x:xs)

--exe 8
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

--exe 9
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = if n > 0 then x : replicate (n-1) x else []

--exe 10
intersperse :: a -> [a] ->[a]
intersperse _ [] = []
intersperse i [a] = [a]
intersperse i (x:xs) = x : i : intersperse i xs

--exe 11
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (==x) xs) : group (dropWhile (==x) xs)

--exe 12
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

--exe 13
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

--exe 14
tails :: [a] -> [[a]]
tails [] = [[]]tails
tails l = l : tails (tail l)

--exe 15
myheads :: [[a]] -> [a]
myheads [[]] = []
myheads (x:xs) = head x : myheads xs

--exe 16
total :: [[a]] -> Int
total [] = 0
total (x:xs) = length x + total xs

--exe 17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):xs) = (x,z) : fun xs 

--exe 18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((s,x,y):xs) = s ++ cola xs

--exe 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((n,nas):xs) = if (a - nas) >= i then n : idade xs else idade a i xs

--exe 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = []
powerEnumFrom n m = n^(m-1) : powerEnumFrom n (m-1)

--exe 21
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False 
            |otherwise True

--exe 22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h1:t1) = h == h1 && h : isPrefixOf t t1

--exe 23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)

--exe 24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf (x:xs) ys


--exe 25
elemIndices :: Eq a => a -> [a] -> [Int] 
elemIndices _ [] = []
elemIndices n l = aux 0 n l 
      where aux i n [] = []
            aux i n (h:t) = if n == h then (i) : aux (i+1) n t else aux (i+1) n t

--exe 26
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = if x `elem` xs then nub xs else x : nub xs

--exe 27 
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (x:xs) = if n == x then xs else x : delete n xs 

--exe 28
(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) l (h:t) = (\\\) (delete h l) t  

--exe 29
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union l (h:t) = if `elem` h l then myunion l t else myunion (l++[h]) t

--exe 30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l 
        |h `elem` l = h : intersect t l 
        |otherwise = intersect t l

--exe 31
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if x <= h then x:h:t else h : insert x t 

--exe 32
unwords :: [String] -> String 
unwords [] = ""
unwords (h:t) = h ++ (if null t then "" else " ") ++ unwords t

--exe 33
unlines :: [String] -> String 
unlines [] = ""
unlines (h:t) = h ++ "\n" ++ unlines t

--exe 34
pMaior :: Ord a => [a] -> Int 
pMaior [n] = 0
pMaior (h:t)
        |h >= (t !! x) = 0
        |otherwise = 1 + x 
        where x = pMaior t 

--exe 35
lookup :: Eq a => a -> [(a,b)] -> Maybe b 
lookup n [] = Nothing
lookup n ((x,xs):t) = if n == x then Just xs else lookup n t 

--exe 36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:s:t) = if h < s then h : preCrescente (s:t) else [h]

--exe 37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--exe 38
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t')
        |h < h' = True
        |h == h' = menor t t'
        |otherwise = False

