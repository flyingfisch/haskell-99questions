getLastElem :: [a] -> a
getLastElem [] = error "Empty List."
getLastElem l = last l

lastButOne :: [a] -> a
lastButOne [] = error "Empty list."
lastButOne [x] = error "Too small."
lastButOne [x, y] = x
lastButOne l = l !! (length l - 2)

elementAt :: [a] -> Int -> a
elementAt [] i = error "Empty list."
elementAt l i
	| i < 1 = head l
	| i > fromIntegral (length l) = last l
	| otherwise = l !! (i - 1)

length' :: [a] -> Int
length' [] = error "Empty list."
length' [x] = 1
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = if reverse' l == l then True else False
