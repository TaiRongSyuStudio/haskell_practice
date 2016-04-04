doubleMe x = x * x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2 

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

numberList1 = [1,2,3,4,5,6]

numberList2 = [7,8,9,10,11,12]

numberListHead x = head x

numberListLast y = last y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum[1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

--Pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element:" ++ show x
tell (x:y:[]) = "The list has two elements:" ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are:" ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0 
length'' (_:xs) = 1 + length'' xs

tail' :: [a] -> [a]
tail' [] = error "The List is empty"
tail' (_:xs) = xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

init' :: [a] -> [a]
init' [] = error "The List is empty"
init' (x:[]) = [x]
init' (x:y:[]) = [x]
init' (x:y) = x : init' y

last' :: [a] -> a
last' [] = error "The List is empty"
last' (x:[]) = x
last' (x:y:[]) = y
last' (x:y) = last' y

--Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
        | weight / (height / 100) ^ 2 <= 18.5 = "You're underweight, you emo, you!"
        | weight / (height / 100) ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
        | weight / (height / 100) ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
        | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b 
     | a > b     = a
     | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
     | a > b     = GT
     | a == b    = EQ
     | otherwise = LT

--Where
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
        | bmi <= skinny = "You're underweight, you emo, you!"
        | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
        | bmi <= fat = "You're fat! Lose some weight, fatty!"
        | otherwise   = "You're a whale, congratulations!"
        where bmi = weight / (height / 100) ^ 2
              skinny = 18.5
              normal = 25.0
              fat    = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
        where bmi weight height = weight / (height / 100) ^ 2

--Let
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
        let sideArea = 2 * pi * r * h;
             topArea = pi * r ^ 2
        in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / (h / 100) ^ 2, bmi >= 25.0]

--Case
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!" 
                       (x:_) -> x

--Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
      | x > maxTail = x
      | otherwise = maxTail
      where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
      | n <= 0 = []
      | otherwise = x : replicate' (n-1) x
      
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
      | n <= 0 = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
     | a == x = True
     | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x];
       biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

--Higher order functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y 

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
       | p x       = x : filter p xs
       | otherwise = filter p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
     let smallerSorted = quicksort (filter (<=x)xs);
          biggerSorted = quicksort (filter (>x)xs)
     in  smallerSorted ++ [x] ++ biggerSorted

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
     | even n = n:chain (n `div` 2)
     | odd  n = n:chain (n*3 + 1)

--Lambda
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15)
                               (map chain [1..100]))

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

--fold
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)
