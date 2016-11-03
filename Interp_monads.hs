module Interp_Monads where
-- Monads

data Term  = Num  Int
           | Bool Bool
           | Var  String
           | Add  Term Term
           | Sub  Term Term
           | Gt   Term Term
           | If   Term Term Term
           | Lam  String Term
           | App  Term Term
           | Put  Int
           | Get
     deriving (Show)

data Value  = I { nOf :: Int }
            | B { bOf :: Bool }
            | F { fOf :: Value -> State Int Value }
            | U { uOf :: () }
--     deriving (Show, Eq)

type Env = [(String, Value)]

lookupEnv                      :: String -> Env -> Value
lookupEnv s []                 = error ("Value not found")
lookupEnv s ((s', v):rest_env) = if s == s' then v else lookupEnv s rest_env

extendEnv         :: String -> Value -> Env -> Env
extendEnv s v env = (s, v):env


-- State
type State s a = s -> (a, s)

-- Run
run :: State Int a -> a
run st = fst(st (0 :: Int))


-- Get
m_read :: State s s
m_read = \s -> (s, s)

-- Put
write   :: s -> State s Value
write x = \s -> (U(()), x)

-- Return
ret   :: a -> State s a
ret x = \s -> (x, s)


-- Bind
m_bind       :: State s a -> (a -> State s b) -> State s b
m_bind  m f  = \s1 ->
                let (x, s2) = m s1
                              in f x s2

interp_m                   :: Term -> Env -> State Int Value
interp_m (Num i) _         = ret (I i)
interp_m (Bool b) _        = ret (B b)
interp_m (Var v) env       = ret (lookupEnv v env)
interp_m (If e0 e1 e2) env = m_bind (interp_m e0 env)
                                     (\v0 -> interp_m (if bOf v0 then e1 else e2) env)
interp_m (Add e1 e2) env   = m_bind (interp_m e1 env)
                                     (\v0 -> (m_bind (interp_m e2 env)
                                                      (\v1 -> (ret (I (nOf v0 + nOf v1))))))
interp_m (Sub e1 e2) env   = m_bind (interp_m e1 env)
                                     (\v0 -> (m_bind (interp_m e2 env)
                                                      (\v1 -> (ret (I (nOf v0 - nOf v1))))))
interp_m (Gt e1 e2) env    = m_bind (interp_m e1 env)
                                    (\v0 -> (m_bind (interp_m e2 env)
                                                    (\v1 -> (ret (B (nOf v0 > nOf v1))))))
interp_m (Lam v e0) env    = ret (F (\a -> (interp_m e0 (extendEnv v a env))))
interp_m (App e0 e1) env   = m_bind (interp_m e0 env)
                                     (\v0 -> m_bind (interp_m e1 env)
                                                     (\v1 -> (fOf v0) v1))
interp_m (Get) env         = m_bind (m_read)
                                    (\v0 -> ret (I v0))
interp_m (Put i) env       = write i

----tests

test :: [Bool]

test = [ 1     == nOf (run (interp_m (Num 1) [])),
         False == bOf (run (interp_m (Bool False) [])),
         True  == bOf (run (interp_m (Gt (Num 3) (Num 2)) [])),
         True  == bOf (run (interp_m (If (Bool True) (Bool True) (Bool False)) [])),
         3     == nOf (run (interp_m (If (Bool False) (Num 1) (Num 3)) [])),
         3     == nOf (run (interp_m (App (Lam "x" (Var "x")) (Num 3)) [])),
         3     == nOf (run (interp_m (Var "x") [("x", (I 3))])),
         5     == nOf (run (interp_m (App (Lam "x" (Get)) (Put 5)) [])) ]

