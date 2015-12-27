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



