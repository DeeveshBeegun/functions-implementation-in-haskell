{-
This funciton is similar to the map function which is found in the prelude library 
of the haskell language. It is a more general function of the two functions bf and df, defined blow, 
which calculates the absolute value and adds a number alternatively. 

propgf xs = bf xs == gf abs xs 
&& df xs == gf (+1) xs 

The original function is defined as follows: 

bf,df :: [Int] → [Int]
bf [ ] = [ ]
bf [x] = [abs x]
bf (x:y:xs) = (abs x) : y : bf xs
df [ ] = [ ]
df [x] = [x + 1]
df (x:y:xs) = (x + 1) : y : df xs

-}

gf :: (a -> a) -> [a] -> [a]
gf f [x] = [f x]
gf f [] = []
gf f (x:y:xs) = f x : y : gf f xs

-- countAces - is a function that returns the number of cards in a given hand which are
-- either aces or jokers. It recursively add the numbers output by the function addAce which returns one if it
-- an Ace e.g. [NormalCard Ace Hearts, NormalCard Ace Clubs, NormalCard Ace Diamonds, Joker, Joker] = 5

data Suit = Hearts | Clubs | Diamonds | Spades deriving Eq
data Rank = Numeric Int | Jack | Queen | King | Ace deriving Eq
data Card = NormalCard Rank Suit | Joker deriving Eq

addJoker :: Card -> Int
addJoker n | n == Joker = 1
           | otherwise = 0

addAce :: Card -> Int
addAce n | n == NormalCard Ace Hearts = 1
         | n == NormalCard Ace Clubs = 1 
         | n == NormalCard Ace Diamonds = 1
         | n == NormalCard Ace Spades = 1 
         | otherwise = 0

countAces :: [Card] -> Int
countAces [] = 0
countAces (n:ns) = addJoker n + addAce n + countAces ns

-- delete - is a function that deletes the first occurence of a value from a list
delete :: Int -> [Int] -> [Int]
findPos :: Eq a => a -> [a] -> [Int]
findPos x xs = [i | (y, i) <- zip xs [0..], x == y]
delete n ns = take(head(findPos n ns)) ns ++ drop(head(findPos n ns) + 1) ns

-- sort - is a function that implements the selection sort algorithm
sort :: [Int] -> [Int]
sort [] = []
sort xs = [foldr1 min xs] ++ sort (delete (foldr1 min xs) xs)

-- cp - is a function that returns the Cartesian Product of a list of lists.
cp :: [[a]] -> [[a]]
cp (x:xss) = [[x'] ++ [y'] | x' <- x, y <- xss, y' <- y]

-- nat2int - is a function that converts a natural number to the corresponding integer

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 