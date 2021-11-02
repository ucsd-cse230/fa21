{-# LANGUAGE DeriveFunctor #-}

module Lec_11_2_21 where

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor)

tree0 :: Tree Char
tree0 = 
  Node 
    (Node 
      (Leaf 'a') 
      (Leaf 'b')) 
    (Node 
      (Leaf 'c') 
      (Leaf 'a'))

treeLabelled :: Tree (Char, Int)
treeLabelled = 
  Node 
    (Node 
      (Leaf ('a',0)) 
      (Leaf ('b',1))) 
    (Node 
      (Leaf ('c',2)) 
      (Leaf ('a',3)))

-- >>> label tree0
-- Node (Node (Leaf ('a',0)) (Leaf ('b',1))) (Node (Leaf ('c',2)) (Leaf ('a',3)))

type State = Int

label :: Tree a -> Tree (a, Int)
label t = fst (worker t 0)

worker :: Tree a -> State -> (Tree (a, Int), State)
worker (Leaf v)   n = (Leaf (v, n),  n')
  where
    n' = n + 1 

worker (Node l r) n = (Node l' r', n'')
  where
    (l', n')  = worker l n
    (r', n'') = worker r n'

-- >>> labelM tree0
-- Node (Node (Leaf ('a',"0")) (Leaf ('b',"1"))) (Node (Leaf ('c',"2")) (Leaf ('a',"3")))

labelM :: Tree a -> Tree (a, String)
labelM t = fst (evalstate 0 (workerM t))

workerM :: Tree a -> ST (Tree (a, String))
workerM (Leaf v) = do 
  n <- tick
  return (Leaf (v, n))

workerM (Node l r) = do 
  l' <- workerM l
  r' <- workerM r
  return (Node l' r')

newtype ST a = STC (State -> (a, State))

instance Functor ST where
  fmap = fmapST 

fmapST :: (a -> b) -> ST a -> ST b
fmapST f (STC sta) = STC (\s -> let (a, s') = sta s in (f a, s')) 


instance Monad ST where
    -- (>>=)  :: ST a -> (a -> ST b) -> ST b
    (>>=) = awesomeST
    -- return :: a -> ST a 
    return = returnST

tick :: ST String
tick = STC (\n -> (show n, n+1))

evalstate :: State -> ST a -> (a, State) 
evalstate s0 (STC sta) = sta s0

returnST :: a -> ST a
returnST a = STC (\s -> (a, s)) 

awesomeST :: ST a -> (a -> ST b) -> ST b
awesomeST (STC sta) a_to_stb = STC 
  (\s -> 
    let (a, s')  = sta s 
        STC stb  = a_to_stb a
        (b, s'') = stb s' 
    in
       (b, s'')
  )

-- >>> evalstate 10 st
-- ([1000,1001,1002],1003)

st :: ST [Int]
st = STC (\n -> ([n, n+1, n+2], n+3))



{- 

-- >>> evalstate 1000 st
-- ([1000,1001,1002],1003)

-- >>> evalstate 100 tick
-- ("100",101)

-- >>> evalstate 100 (tick >>= \_ -> return "mugatu")
-- ("mugatu",101)

-- >>> evalstate 100 (do { n0 <- tick; n1 <- tick; n2 <- tick; return [n0,n1,n2] })
-- (["100","101","102"],103)

--(A) z = "100"
--(B) z = "101"
--(C) z = "102"


-- >>> evalstate 100 (tick >>= \z1 -> return [z1])

-- >>> evalstate 0 (do { z0 <- tick; _ <- tick; z2 <- tick; return (z0 ++ z2) })
-- ("02",3)
-- ("100102",103)



-}














instance Applicative ST where
  pure = undefined
  (<*>) = undefined