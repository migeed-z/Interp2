module Interp_ML_Monads where
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
           | Put  Term Term
           | Get  Term
           | Ref  Term
     deriving (Show)

data Value  = I { nOf :: Int }
            | B { bOf :: Bool }
            | F { fOf :: Value -> State Value Value }
            | U { uOf :: () }
            | L { lOf :: Int }
--     deriving (Show, Eq)

type Env = [(String, Value)]

lookupEnv                      :: String -> Env -> Value
lookupEnv s []                 = error ("Value not found")
lookupEnv s ((s', v):rest_env) = if s == s' then v else lookupEnv s rest_env

extendEnv         :: String -> Value -> Env -> Env
extendEnv s v env = (s, v):env


-- State
type State s a = [s] -> (a, [s])

-- Run
m_run :: State Value a -> a
m_run st = fst(st ([] :: [Value]))

-- Get
m_read   :: Int -> State s s
m_read i = \s -> ((case (index s i) of
                        Just value -> value
                        Nothing    -> error "Get index out of bound"), s)

-- Ref
m_alloc :: s -> State s Value
m_alloc v = \s -> ( L (length s), s ++ [v])

-- Put
m_write   :: s -> Int -> State s Value
m_write v i = \s -> (U(()), replace_nth i v s)

-- Return
m_ret   :: a -> State s a
m_ret x = \s -> (x, s)

index::[a]-> Int-> Maybe a
index (x:xs) 0 = Just x
index (x:xs) n | n > 0 = index xs (n-1)
index _ _ = Nothing



-- Bind
m_bind       :: State s a -> (a -> State s b) -> State s b
m_bind  m f  = \s1 ->
                let (x, s2) = m s1
                              in f x s2


-- Replace nth
replace_nth 0 newVal (x:xs) = newVal:xs
replace_nth n newVal (x:xs) = x:(replace_nth (n-1) newVal xs)


interp_m                   :: Term -> Env -> State Value Value
interp_m (Num i) _         = m_ret (I i)
interp_m (Bool b) _        = m_ret (B b)
interp_m (Var v) env       = m_ret (lookupEnv v env)
interp_m (If e0 e1 e2) env = m_bind (interp_m e0 env)
                                     (\v0 -> interp_m (if bOf v0 then e1 else e2) env)
interp_m (Add e1 e2) env   = m_bind (interp_m e1 env)
                                     (\v0 -> (m_bind (interp_m e2 env)
                                                      (\v1 -> (m_ret (I (nOf v0 + nOf v1))))))
interp_m (Sub e1 e2) env   = m_bind (interp_m e1 env)
                                     (\v0 -> (m_bind (interp_m e2 env)
                                                      (\v1 -> (m_ret (I (nOf v0 - nOf v1))))))
interp_m (Gt e1 e2) env    = m_bind (interp_m e1 env)
                                    (\v0 -> (m_bind (interp_m e2 env)
                                                    (\v1 -> (m_ret (B (nOf v0 > nOf v1))))))
interp_m (Lam v e0) env    = m_ret (F (\a -> (interp_m e0 (extendEnv v a env))))
interp_m (App e0 e1) env   = m_bind (interp_m e0 env)
                                     (\v0 -> m_bind (interp_m e1 env)

                                                     (\v1 -> (fOf v0) v1))

interp_m (Ref e) env       = m_bind (interp_m e env) (\v -> m_alloc v)

interp_m (Get e) env       = m_bind (interp_m e env) (\v -> m_read (lOf v))

interp_m (Put e1 e2) env   = m_bind (interp_m e1 env)
                             (\v1 -> (m_bind (interp_m e2 env)
                             (\v2 ->  m_write v2 (lOf v1))))



----tests----

test :: [Bool]

test = [ 1     == nOf (m_run (interp_m (Num 1) [])),
         False == bOf (m_run (interp_m (Bool False) [])),
         True  == bOf (m_run (interp_m (Gt (Num 3) (Num 2)) [])),
         True  == bOf (m_run (interp_m (If (Bool True) (Bool True) (Bool False)) [])),
         3     == nOf (m_run (interp_m (If (Bool False) (Num 1) (Num 3)) [])),
         3     == nOf (m_run (interp_m (App (Lam "x" (Var "x")) (Num 3)) [])),
         3     == nOf (m_run (interp_m (Var "x") [("x", (I 3))])),
         5     == nOf (m_run (interp_m (Get (Ref (Num 5))) [])),
         5     == nOf (m_run (interp_m (App (Lam "x" (App (Lam "y" (Get (Var "x")))
                                                        (Put (Var "x") (Num 5))))
                                          (Ref (Num 1))) [])),
         6     == nOf (m_run (interp_m (App (Lam "z" (Add (Var "z") (Var "z")))
                                          (App (Lam "x" (App (Lam "y" (Get (Var "x")))
                                                             (Put (Var "x") (Num 3))))
                                               (Ref (Num 1)))) [])),
         True  == bOf (m_run (interp_m (Get (Ref (Bool True))) [])),
         True  == bOf (m_run (interp_m (App (Lam "x" (Get (Var "x"))) (Ref (Bool True))) []))]

