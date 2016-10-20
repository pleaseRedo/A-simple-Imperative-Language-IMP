------------------------- State

type Variable = String

type State = [(Variable,Integer)]


empty :: State
empty = []


get :: State -> Variable -> Integer
get [] _ = 0
get ((x,n):xs) y | x == y    = n
                 | otherwise = get xs y


set :: Variable -> Integer -> State -> State
set x n s = (x,n) : del x s
  where
    del _ [] = []
    del y (x:xs) | fst x == y = xs
                 | otherwise  = x : del y xs

------------------------- Input-Output streams

type Stream = [Integer]

------------------------- Part 2

data Aexp = Num Integer
          | Var Variable
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp
          | Next

evalA :: Aexp -> State -> Stream -> (Integer,Stream)
evalA (Num n)     s (x:xs) = (n,x:xs)
evalA (Num n)     s []     = (n,[])
evalA (Var v)     s (x:xs) = (get s v,x:xs)
evalA (Var v)     s []     = (get s v,[])

evalA (a :+: b)   s ls = (y + z,zs)
  where
    (y,ys) = evalA a s ls
    (z,zs) = evalA b s ys

evalA (a :*: b)   s ls = (y * z,zs)
  where
    (y,ys) = evalA a s ls
    (z,zs) = evalA b s ys

evalA (a :-: b)   s ls = (y - z,zs)
  where
    (y,ys) = evalA a s ls
    (z,zs) = evalA b s ys


evalA Next s (x:xs) = (x,xs)


a1 :: Aexp
a1 =  Next :*: Num 6 :+: Num 3
a2 :: Aexp
a2 =  Num 6 :+: Next
as :: Stream
as = [1,2,3,4,5,6,7]


------------------------- Part 3

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp
          | Bexp :&&: Bexp
          | Bexp :||: Bexp

evalB :: Bexp -> State -> Stream -> (Bool,Stream)
evalB (Boolean b) _ (x:xs) = (b,x:xs)
evalB (Boolean b) _ [] = (b,[])
evalB (a :==: b)  s ls = (y == z, zs)
  where
    (y,ys) = evalA a s ls
    (z,zs) = evalA b s ys

evalB (a :<=: b)  s ls = (y <= z, zs)
  where
    (y,ys) = evalA a s ls
    (z,zs) = evalA b s ys

evalB (Neg b)     s ls = (not y,ys)
  where
    (y,ys) = evalB b s ls

evalB (a :&: b)   s ls = (y && z,zs)
  where
    (y,ys) = evalB a s ls
    (z,zs) = evalB b s ys


evalB (a :|: b)   s ls = (y || z,zs)
  where
    (y,ys) = evalB a s ls
    (z,zs) = evalB b s ys

evalB (a :&&: b)  s ls | y = evalB b s ys
                       | otherwise = (False, ls)
                         where
                          (y,ys) = evalB a s ls

evalB (a :||: b)  s ls | y = (True,ys)
                       | otherwise = evalB b s ls
                          where
                           (y,ys) = evalB a s ls

b1 :: Bexp
b1 = ((Num 100 :<=: Next) :&: (Next :==: Next)) :|: (Num 4 :==: Next)
b2 :: Bexp
b2 = ((Num 100 :<=: Next) :&&: (Next :==: Next)) :|: (Num 4 :==: Next)
bs :: Stream
bs = [1,2,3,4,5,6]


------------------------- Part 4

data Comm = Skip
          | Variable :=: Aexp
          | Comm :>: Comm
          | If Bexp Comm Comm
          | While Bexp Comm
          | Print Aexp

evalC :: Comm -> State -> Stream ->(State,Stream,Stream)
evalC Skip        s (x:xs) = (s,[],(x:xs))
evalC Skip        s []     = (s,[],[])

evalC (v :=: a)   s ls = (set v y s,[],ys)        --where x = evalA a s
                             where
                               (y , ys)= evalA a s ls
evalC (c :>: d)   s ls = (u, xs ++ ys , is)         where (t,xs,hs) = evalC c s ls
                                                          (u,ys,is) = evalC d t hs

evalC (If b c d)  s ls| fst (evalB b s ls) = evalC c s ys
                          | otherwise = evalC d s ys
                          where
                            (y,ys) = evalB b s ls
evalC (While b c) s ls| fst (evalB b s ls) = (u, ys ++ zs, yz)
                          | otherwise = (s,[],ls)
                          where (h,hs)    = evalB b s ls
                                (t,ys,yg) = evalC c s hs
                                (u,zs,yz) = evalC (While b c) t yg
evalC (Print a)   s ls = (s, [fst (evalA a s ls)],ys)
      where
        (y,ys) = evalA a s ls




factorial :: Comm
factorial = While (Boolean True) (
            ("x" :=: Next)  :>:
            ("y" :=: Num 1) :>:
            While (Num 1 :<=: Var "x") (
                ("y" :=: (Var "x" :*: Var "y")) :>:
                ("x"  :=: (Var "x" :-: Num 1))
            ) :>:
            Print (Var "y") )

--runFactorial [Stream] will return answer with an exception as specified from cw spec.
runFactorial :: Stream -> Stream
runFactorial a = out
  where
    (_,out,_) = evalC factorial empty a
------------------------- Part 1

-- snd (evalC c1/c2 empty) no longer work since I change the output style
c1 :: Comm
c1 = While (Boolean True) (
     Print(Num 0))

c2 :: Comm
c2 =  ("x" :=: Num 1) :>:
     While (Boolean True) (
      (Print (Var "x")) :>: ("x" :=: (Var "x" :+: Num 1))
      )


------------------------- Part 5

data Chain = Comm :>>: Comm
evalD :: Chain -> Stream -> Stream
evalD (a :>>: b) ls = out
  where
    (_,xs,_) = evalC a empty ls
    (_,out,_)  = evalC b empty xs

d1 :: Chain
d1 = c2 :>>: factorial
