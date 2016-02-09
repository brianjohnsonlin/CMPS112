{-
 - Program: hw2.hs
 - Authors: Yunyi Ding and Brian Lin
 - On this homework, we worked together for 5 hours,
 - Yunyi worked independently for 3 hours,
 - and Brian worked independently for 3 hours.
-}

-- This function behaves like the standard foldl function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op a [] = a
myFoldl op a (x:xs) = myFoldl op (op a x) xs

-- This function behaves like the standard reverse function
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- This function behaves like the standard foldr function
-- It uses the foldl function
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a bs = foldl (\g b x -> g (f b x)) id bs a
--myFoldr op n [] = n
--myFoldr op n list = myFoldr op (op (last list) n) (init list)

-- This function behaves like the standard foldl function
-- It uses the foldr function
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f a bs = foldr (\b g x -> g (f x b)) id bs a

-- This function returns true if the Char is an uppercase letter and false if otherwise
isUpper :: Char -> Bool
isUpper c = elem c ['A'..'Z']

-- This function returns only the capital letters of the provided string
-- It uses the filter function
onlyCapitals1 :: String -> String
onlyCapitals1 s = filter isUpper s

-- This function returns only the capital letters of the provided string
-- It uses list comprehension
onlyCapitals2 :: String -> String
onlyCapitals2 s = [x | x <- s, isUpper x ]

-- This function returns only the capital letters of the provided string
-- It uses recursion
onlyCapitals3 :: String -> String
onlyCapitals3 [] = ""
onlyCapitals3 (x:xs)
	| isUpper x = [x] ++ onlyCapitals3 xs
	| otherwise = onlyCapitals3 xs

-- This function behaves like the standard divMod function which returns a tuple with
-- the quotient and the remainder of an integer division of the provided two numbers
divRemainder :: Int -> Int -> (Int, Int)
divRemainder a b = (a `div` b, a `mod` b)

-- This function returns the sum of the digits of the given integer
digitSum :: Int -> Int
digitSum a
	| a > 9 = read [head (show a)] + digitSum (read (tail (show a)))
	| otherwise = a

-- This function takes a string of digits and spells out the number as a string in English
sayNum :: String -> String
sayNum "0" = "zero "
sayNum "1" = "one "
sayNum "2" = "two "
sayNum "3" = "three "
sayNum "4" = "four "
sayNum "5" = "five "
sayNum "6" = "six "
sayNum "7" = "seven "
sayNum "8" = "eight "
sayNum "9" = "nine "
sayNum "10" = "ten "
sayNum "11" = "eleven "
sayNum "12" = "twelve "
sayNum "13" = "thirteen "
sayNum "14" = "fourteen "
sayNum "15" = "fifteen "
sayNum "16" = "sixteen "
sayNum "17" = "seventeen "
sayNum "18" = "eighteen "
sayNum "19" = "nineteen "
sayNum "20" = "twenty "
sayNum "30" = "thirty "
sayNum "40" = "forty "
sayNum "50" = "fifty "
sayNum "60" = "sixty "
sayNum "70" = "seventy "
sayNum "80" = "eighty "
sayNum "90" = "ninety "
sayNum "00" = ""
sayNum s
	| length (filter isNotDigit s) > 0 = "Invalid Input"
	| length s == 0 = "Invalid Input"
	| length s > 3 = sayNumH (take (takeDigits (length s)) s) (((length s)-1) `div` 3) ++ sayNum (reverse (take ((length s) - (takeDigits (length s))) (reverse s)))
	| length s == 3 = sayNumH s 0
	| length s == 2 = sayNum ([head s] ++ "0") ++ sayNum [last s]

-- sayNum helper function
sayNumH :: String -> Int -> String
sayNumH s i
	| length s == 0 = ""
	| head s == '0' = sayNumH (tail s) i
	| length s == 3 = sayNum [head s] ++ "hundred " ++ sayNum(tail s) ++ illionName i
	| otherwise = sayNum s ++ illionName i

-- sayNum helper function: determines which "illion" to use
illionName :: Int -> String
illionName 0 = ""
illionName 1 = "thousand "
illionName 2 = "million "
illionName 3 = "billion "
illionName 4 = "trillion "
illionName 5 = "quadrillion "
illionName 6 = "quintillion "
illionName 7 = "sextillion "
illionName 8 = "septillion "
illionName 9 = "octillion "
illionName 10 = "nonillion "
illionName 11 = "decillion "
illionName 12 = "undecillion "
illionName 13 = "duodecillion "
illionName 14 = "tredecillion "
illionName 15 = "quattuordecillion "
illionName 16 = "quindecillion "
illionName 17 = "sexdecillion "
illionName 18 = "deptendecillion "
illionName 19 = "octodecillion "
illionName 20 = "novemdecillion "
illionName 21 = "vigintillion "++
+

-- sayNum helper function: determines how many digits to separate from the head of the string
takeDigits :: Int -> Int
takeDigits d
	| m == 0 = 3
	| otherwise = m
	where m = d `mod` 3

-- sayNum helper function: returns true if the Char is not a digit and false if ot9herwise
isNotDigit :: Char -> Bool
isNotDigit c = not (elem c ['0'..'9'])
