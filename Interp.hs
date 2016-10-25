module Interp where

-- create another term language for monads and add read/write
data Term = Num Int
          | Bool Bool
          | Var String
          | Add Term Term
          | Sub Term Term
          | Gt  Term Term
          | If  Term Term Term
          | Lam String Term
          | App Term Term
     deriving (Show, Eq)
     
data Value = I { nOf :: Int }
           | B { bOf :: Bool }
           | F { fOf :: Value -> Value }

instance Show Value where
  show (I i) = show i
  show (B b) = show b
 
type Env = [(String, Value)]
        
lookupEnv :: String -> Env  -> Value
lookupEnv s [] = error ("Value not found")
lookupEnv s ((s', v):rest_env) = if s == s' then v else lookupEnv s rest_env

extendEnv :: String -> Value -> Env -> Env 
extendEnv s v env = (s, v):env
--fix var names
interp :: Term -> Env -> Value
interp (Num i) _ = (I i)
interp (Bool b) _ = (B b)
interp (Var v) env = lookupEnv v env
interp (If e0 e1 e2) env = interp (if bOf (interp e0 env) then e1 else e2) env
interp (Add e1 e2) env = I (nOf (interp e1 env) + nOf (interp e2 env))
interp (Sub e1 e2) env = I (nOf (interp e1 env) - nOf (interp e2 env))
interp (Gt e1 e2) env = B (nOf (interp e1 env) > nOf (interp e2 env))
interp (Lam v e0)    env = F (\a -> interp e0 (extendEnv v a env))
interp (App e0 e1)   env = fOf (interp e0 env) (interp e1 env)


-- tests

test0 = [ 1  == nOf (interp (Num 1) []),
          False == bOf (interp (Bool False) []),
          True  == bOf (interp (Gt (Num 3) (Num 2)) []),
          True  == bOf (interp (If (Bool True) (Bool True) (Bool False)) []),
          3     == nOf (interp (If (Bool False) (Num 1) (Num 3)) []),
          3     == nOf (interp (App (Lam "x" (Var "x")) (Num 3)) []),
          3     == nOf (interp (Var "x") [("x", (I 3))])]

---------------------------------------------------------------------------

-- Monads

data Term2 = Num Int
          | Bool Bool
          | Var String
          | Add Term2 Term2
          | Sub Term2 Term2
          | Gt  Term2 Term2
          | If  Term2 Term2 Term2
          | Lam String Term2
          | App Term2 Term2
     deriving (Show, Eq)
     
data Value2 = I { nOf :: Int }
            | B { bOf :: Bool }
            | F { fOf :: Value -> Value }


-- State
type State s a = s -> (a, s)

-- Get
read :: State s s
read = \s -> (s, s)

-- Put
write :: s -> State s ()
write x = \s -> ((), x)

-- Return
ret :: a -> State s a
ret x = \s -> (x, s)


-- Bind
my_bind :: State s a -> (a -> State s b) -> State s b
my_bind  m f = \s1 ->
  let (x, s2) = m s1
  in f x s2 

interp_m :: Term -> Env -> State Value Value
interp_m (Num i) _ = ret (I i)
interp_m (Bool b) _ = ret (B b)
interp_m (Var v) env = ret (lookupEnv v env)
interp_m (If e0 e1 e2) env = my_bind (interp_m e0 env)
  (\v0 -> interp_m (if bOf v0 then e1 else e2) env)
interp_m (Add e1 e2) env = ret (I (nOf (interp_m e1 env) + nOf (interp_m e2 env)))



-- interp_m (Add e1 e2) env = I (nOf (interp_m e1 env) + nOf (interp_m e2 env))
-- interp_m (Sub e1 e2) env = I (nOf (interp_m e1 env) - nOf (interp_m e2 env))
-- interp_m (Gt e1 e2) env = B (nOf (interp_m e1 env) > nOf (interp_m e2 env))
-- interp_m (Lam v e0)    env = F (\a -> interp_m e0 (extendEnv v a env))
-- interp_m (App e0 e1)   env = fOf (interp_m e0 env) (interp_m e1 env)


