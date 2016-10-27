module Interp_Monads where
-- Monads

data Term  = Num Int
           | Bool Bool
           | Var String
           | Add Term Term
           | Sub Term Term
           | Gt  Term Term
           | If  Term Term Term
           | Lam String Term
           | App Term Term
     deriving (Show, Eq)

data Value  = I { nOf :: Int }
            | B { bOf :: Bool }
            | F { fOf :: Value -> Value }


type Env = [(String, Value)]

lookupEnv                      :: String -> Env  -> Value
lookupEnv s []                 = error ("Value not found")
lookupEnv s ((s', v):rest_env) = if s == s' then v else lookupEnv s rest_env

extendEnv         :: String -> Value -> Env -> Env
extendEnv s v env = (s, v):env


-- State
type State s a = s -> (a, s)

-- Get
read :: State s s
read = \s -> (s, s)

-- Put
write   :: s -> State s ()
write x = \s -> ((), x)

-- Return
ret   :: a -> State s a
ret x = \s -> (x, s)


-- Bind
my_bind       :: State s a -> (a -> State s b) -> State s b
my_bind  m f  = \s1 ->
                let (x, s2) = m s1
                              in f x s2

interp_m                   :: Term -> Env -> State Value Value
interp_m (Num i) _         = ret (I i)
interp_m (Bool b) _        = ret (B b)
interp_m (Var v) env       = ret (lookupEnv v env)
interp_m (If e0 e1 e2) env = my_bind (interp_m e0 env)
                                     (\v0 -> interp_m (if bOf v0 then e1 else e2) env)
interp_m (Add e1 e2) env   = my_bind (interp m e1 env)
                                      (\v0 -> nOf v0 + nOf (interp_m e2 env))
-- interp_m (Sub e1 e2) env   = my_bind (interp m e1 env)
--                                      (\v0 -> nOf v0 - nOf (interp_m e2 env))
-- interp_m (Gt e1 e2) env    = my_bind (interp m e1 env)
--                                      (\v0 -> nOf v0 > nOf (interp_m e2 env))
-- interp_m (Lam v e0) env    = ret (\a -> interp_m e0 (extendEnv v a env))
-- interp_m (App e0 e1) env   = my_bind (interp_m e0 env)
--                                      (\v0 -> fOf v0 (interp_m e1 env))


-- interp_m (Add e1 e2) env   = I (nOf (interp_m e1 env) + nOf (interp_m e2 env))
-- interp_m (Sub e1 e2) env   = I (nOf (interp_m e1 env) - nOf (interp_m e2 env))
-- interp_m (Gt e1 e2) env    = B (nOf (interp_m e1 env) > nOf (interp_m e2 env))
-- interp_m (Lam v e0) env    = F (\a -> interp_m e0 (extendEnv v a env))
-- interp_m (App e0 e1)env    = fOf (interp_m e0 env) (interp_m e1 env)
