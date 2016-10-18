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
     deriving (Show)
   
  

data Value = String | Num | Bool
        
  

type Env Value = [(String, Value)]
        


emptyEnv :: Env Value


lookupEnv :: String Env Value -> Value
lookupEnv s [] = error ("Value not found")
lookupEnv s ((s', v):rest_env) = if s == s' then v else lookupEnv s rest_env

extendEnv :: String -> Value -> Env Value -> Env Value
extendEnv s v env = (s, v):env

interp :: Term -> Env Value -> Value
interp (Num i) _ = i








