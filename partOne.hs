-- Problem 1 --
myLast [] = error "Liste vide"
myLast [x] = x
myLast (_:suite) = myLast suite

-- Problem 2 --
myButLast [] = error "Liste vide"
myButLast [_] = error "Pas assez d'éléments"
myButLast (t:q) = if length q > 2 then myButLast q else q !! 0

-- Problem 3 --
elementAt [] x = error "Liste vide"
elementAt (t:_) 1 = t
elementAt (_:xs) y = if y < 1 then error "Invalide"
                     else elementAt xs (y - 1)

 -- Problem 4 --
myLength [] = 0
myLength [x] = 1
myLength (_:q) = 1 + (myLength q)

-- Problem 5 --
myReverse [] = []
myReverse [x] = [x]
myReverse (t:q) = (myReverse q) ++ [t]

-- Problem 6 --
isPalindrome x = x == (myReverse x)

-- Problem 7 --
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List (t:q)) = (flatten t) ++ (flatten (List q))
flatten (List []) = []

-- Problem 8 --
compress [] = []
compress [x] = [x]


main = print (compress "aaaaabbbbbcccccaaaaddddddeeeeeee")
