module Lec_10_26_21 where

import Text.Printf (printf)

incr :: Int -> Int
incr x = x + 1


data List a = Nil | Cons a (List a)
  deriving (Show)
  
instance Functor List where

instance Applicative List where

instance Monad List where
    return x          = Cons x Nil
    Nil >>= k         = Nil
    (Cons x xs) >>= k = k x `append` (xs >>= k)

boo :: List (Integer, Bool)
boo = do
    x <- Cons 1 (Cons 2 Nil)
    y <- Cons True (Cons False Nil)
    return (x, y)

-- >>> boo
-- Cons (1,True) (Cons (1,False) (Cons (2,True) (Cons (2,False) (Cons (3,True) (Cons (3,False) Nil)))))

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)