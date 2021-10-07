module Hello where

thing1 :: Integer
thing1 = 2 

thing2 :: Double
thing2 = 5.1 + 6.9

thing3 :: [Char]
thing3 = ['c', 'a', 't'] 

blob :: [Integer]
blob = [1,2,3,4]

l1, l2, l3 :: [Integer]
l1 = [1,2,3]
l2 = [4,5,6]
l3 = [7,8,9]

flob :: [[Integer]]
flob = [l1, l2, l3]

-- >>> sumTo 5
-- 15


sumTo :: Int -> Int
sumTo n = if n == 0 then 0 else n + sumTo (n-1)

-- isitpossible :: [String]
-- isitpossible = [1, "cat"]




{- 

flob :: 
flob = [  , [4,5,6], [7,8,9] ]


-}

thing4 :: [Char]
thing4 = "dog"

thing5 :: [Char]
thing5 = thing4 ++ thing5