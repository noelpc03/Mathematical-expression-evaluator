module Evaluator (eval, safeEval, derivative) where

import Parser (Expr(..))
import Prelude hiding (tan, cos, sin)
import qualified Prelude as P
import qualified Control.Exception as Exception
import Control.Exception (SomeException, evaluate)

-- Función para evaluar una expresión
eval :: Expr -> Double
eval (Val n)     = n
eval (Add x y)   = eval x + eval y
eval (Sub x y)   = eval x - eval y
eval (Mul x y)   = eval x * eval y
eval (Div x y)   = eval x / eval y
eval (Sqrt x)    = sqrt (eval x)
eval (Pow x y)   = eval x ** eval y
eval (Sin x)     = P.sin (eval x)
eval (Cos x)     = P.cos (eval x)
eval (Tan x)     = P.tan (eval x)
eval (Cot x)     = 1 / P.tan (eval x)
eval (ASin x)    = P.asin (eval x)
eval (ACos x)    = P.acos (eval x)
eval (ATan x)    = P.atan (eval x)
eval (ACot x)    = P.pi / 2 - P.atan (eval x)
eval (Deriv expr var) = eval (derivative expr var)
eval (Integrate expr var) = eval (integral expr var)

-- **Derivada simbólica**
derivative :: Expr -> String -> Expr
derivative (Val _) _ = Val 0
derivative (Var v) var
    | v == var  = Val 1
    | otherwise = Val 0
derivative (Add u v) var = Add (derivative u var) (derivative v var)
derivative (Sub u v) var = Sub (derivative u var) (derivative v var)
derivative (Mul u v) var = Add (Mul (derivative u var) v) (Mul u (derivative v var))
derivative (Div u v) var = Div (Sub (Mul (derivative u var) v) (Mul u (derivative v var))) (Pow v (Val 2))
derivative (Pow u (Val n)) var = Mul (Mul (Val n) (Pow u (Val (n - 1)))) (derivative u var)
derivative (Sin u) var = Mul (Cos u) (derivative u var)
derivative (Cos u) var = Mul (Val (-1)) (Mul (Sin u) (derivative u var))
derivative _ _ = error "Derivada no soportada para esta expresión"

-- **Integral simbólica**
integral :: Expr -> String -> Expr
integral (Val n) var = Mul (Val n) (Var var)  -- ∫c dx = c*x
integral (Var v) var
    | v == var  = Div (Pow (Var v) (Val 2)) (Val 2)  -- ∫x dx = x² / 2
    | otherwise = Mul (Var v) (Var var)  -- ∫y dx = yx
integral (Add u v) var = Add (integral u var) (integral v var)
integral (Sub u v) var = Sub (integral u var) (integral v var)
integral (Mul (Val n) (Var v)) var
    | v == var  = Mul (Val n) (Div (Pow (Var v) (Val 2)) (Val 2))  -- ∫a*x dx = (a*x²)/2
    | otherwise = Mul (Val n) (Mul (Var v) (Var var))  -- ∫a*y dx = a*y*x
integral _ _ = error "Integral no soportada para esta expresión"

-- Evaluación segura que captura excepciones
safeEval :: Expr -> IO (Either String Double)
safeEval expr = do
    result <- Exception.try (evaluate (eval expr)) :: IO (Either SomeException Double)
    return $ case result of
        Left ex  -> Left ("Error al evaluar la expresión: " ++ show ex)
        Right val -> Right val
        