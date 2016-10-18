module Interp where

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

interp :: Term -> Env -> Value
interp (Num i) _ = (I i)
interp (Bool b) _ = (B b)
interp (Var v) env = lookupEnv v env
interp (If e0 e1 e2) env = interp (if bOf (interp e0 env) then e1 else e2) env
interp (Add e1 e2) env = I (nOf (interp e1 env) + nOf (interp e2 env))
interp (Sub e1 e2) env = I (nOf (interp e1 env) - nOf (interp e2 env))
interp (Gt e1 e2) env = B (nOf (interp e1 env) > nOf (interp e2 env))

-- wat
interp (Lam v e0)    env = F (\a -> interp e0 (extendEnv v a env))
interp (App e0 e1)   env = fOf (interp e0 env) (interp e1 env)



test0 = [ 1  == nOf (interp (Num 1) []),
          False == bOf (interp (Bool False) []),
          True  == bOf (interp (Gt (Num 3) (Num 2)) []),
          True  == bOf (interp (If (Bool True) (Bool True) (Bool False)) []),
          3     == nOf (interp (If (Bool False) (Num 1) (Num 3)) []),
          3     == nOf (interp (App (Lam "x" (Var "x")) (Num 3)) []),
          3     == nOf (interp (Var "x") [("x", (I 3))])]
