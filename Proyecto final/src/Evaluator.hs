module Evaluator (eval, safeEval, derivative, exprToString) where

import Parser (Expr(..))
import Prelude hiding (tan, cos, sin)
import qualified Prelude as P
import qualified Control.Exception as Exception
import Control.Exception (SomeException, evaluate)

-- Función para evaluar una expresión y simplificarla
eval :: Expr -> Expr
eval (Val n)     = Val n
eval (Var v)     = Var v
eval (Add x y)   = simplifyAdd (eval x) (eval y)
eval (Sub x y)   = simplifySub (eval x) (eval y)
eval (Mul x y)   = simplifyMul (eval x) (eval y)
eval (Div x y)   = simplifyDiv (eval x) (eval y)
eval (Sqrt x)    = simplifySqrt (eval x)
eval (Pow x y)   = simplifyPow (eval x) (eval y)
eval (Sin x)     = simplifySin (eval x)
eval (Cos x)     = simplifyCos (eval x)
eval (Tan x)     = simplifyTan (eval x)
eval (Cot x)     = simplifyCot (eval x)
eval (ASin x)    = simplifyASin (eval x)
eval (ACos x)    = simplifyACos (eval x)
eval (ATan x)    = simplifyATan (eval x)
eval (ACot x)    = simplifyACot (eval x)
eval (Deriv expr var) = eval (derivative expr var)
eval (DerivEval expr var point) = 
    let derivedExpr = derivative expr var
        substitutedExpr = substituteVars derivedExpr [(var, point)]
    in eval substitutedExpr
eval (Integrate expr var) = eval (integral expr var)

-- Función para convertir una expresión a una cadena de texto
exprToString :: Expr -> String
exprToString (Val n) = show n
exprToString (Var v) = v
exprToString (Add x y) = "(" ++ exprToString x ++ " + " ++ exprToString y ++ ")"
exprToString (Sub x y) = "(" ++ exprToString x ++ " - " ++ exprToString y ++ ")"
exprToString (Mul x y) = exprToString x ++ " * " ++ exprToString y
exprToString (Div x y) = exprToString x ++ " / " ++ exprToString y
exprToString (Sqrt (Val n)) = "sqrt(" ++ show n ++ ")"
exprToString (Pow (Var n) (Val e)) = "pow(" ++ show n ++ "," ++ show e ++ ")"

-- funciones trigonometricas
exprToString (Sin (Val n)) = "sin(" ++ show n ++ ")"
exprToString (Cos (Val n)) = "cos(" ++ show n ++ ")"
exprToString (Tan (Val n)) = "tan(" ++ show n ++ ")"
exprToString (Cot (Val n)) = "cot(" ++ show n ++ ")"
exprToString (ASin (Val n)) = "asin(" ++ show n ++ ")"
exprToString (ACos (Val n)) = "acos(" ++ show n ++ ")"
exprToString (ATan (Val n)) = "atan(" ++ show n ++ ")"
exprToString (ACot (Val n)) = "acot(" ++ show n ++ ")"


-- Casos para funciones trigonométricas y valores
exprToString (Sin x) = "sin(" ++ exprToString x ++ ")"
exprToString (Cos x) = "cos(" ++ exprToString x ++ ")"
exprToString (Tan x) = "tan(" ++ exprToString x ++ ")"
exprToString (Cot x) = "cot(" ++ exprToString x ++ ")"
exprToString (ASin x) = "asin(" ++ exprToString x ++ ")"
exprToString (ACos x) = "acos(" ++ exprToString x ++ ")"
exprToString (ATan x) = "atan(" ++ exprToString x ++ ")"
exprToString (ACot x) = "acot(" ++ exprToString x ++ ")"

-- operaciones complejas
exprToString (Integrate expr var) = "integrate(" ++ exprToString expr ++ ", " ++ var ++ ")"
exprToString (Deriv expr var) = 
    "deriv(" ++ exprToString expr ++ ", " ++ var ++ ")"
exprToString (DerivEval expr var point) = 
    "deriv(" ++ exprToString expr ++ ", " ++ var ++ ", " ++ show point ++ ")"

-- 
exprToString x = error $ "Expresión no soportada en exprToString: " ++ show x

-- Simplificación de suma
simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Val 0) y = y
simplifyAdd x (Val 0) = x
simplifyAdd (Val a) (Val b) = Val (a + b)
simplifyAdd x y = Add x y

-- Simplificación de resta
simplifySub :: Expr -> Expr -> Expr
simplifySub x (Val 0) = x
simplifySub (Val a) (Val b) = Val (a - b)
simplifySub x y = Sub x y

-- Simplificación de multiplicación
simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Val 0) _ = Val 0
simplifyMul _ (Val 0) = Val 0
simplifyMul (Val 1) y = y
simplifyMul x (Val 1) = x
simplifyMul (Val a) (Val b) = Val (a * b)
simplifyMul x y = Mul x y

-- Simplificación de división
simplifyDiv :: Expr -> Expr -> Expr
simplifyDiv x (Val 1) = x
simplifyDiv (Val a) (Val b) = Val (a / b)
simplifyDiv x y = Div x y

-- Simplificación de potencia
simplifyPow :: Expr -> Expr -> Expr
simplifyPow x (Val 1) = x
simplifyPow (Val a) (Val b) = Val (a ** b)
simplifyPow x y = Pow x y

-- Simplificación de raíz cuadrada
simplifySqrt :: Expr -> Expr
simplifySqrt (Val n) 
    | n >= 0    = Val (sqrt n)
    | otherwise = error "Raíz cuadrada de número negativo"
simplifySqrt (Pow x (Val 2)) = x  -- sqrt(x²) = |x|
simplifySqrt (Mul (Val a) (Val b)) = Val (sqrt (a * b))
simplifySqrt x = Sqrt x

-- Simplificación de funciones trigonométricas
simplifySin :: Expr -> Expr
simplifySin (Val 0) = Val 0
simplifySin (Val n) = Val (P.sin n)
simplifySin x = Sin x

simplifyCos :: Expr -> Expr
simplifyCos (Val 0) = Val 1
simplifyCos (Val n) = Val (P.cos n)
simplifyCos x = Cos x

simplifyTan :: Expr -> Expr
simplifyTan (Val 0) = Val 0
simplifyTan (Val n) = Val (P.tan n)
simplifyTan x = Tan x

simplifyCot :: Expr -> Expr
simplifyCot (Val 0) = error "Cotangente de 0 no definida"
simplifyCot (Val n) = Val (1 / P.tan n)
simplifyCot x = Cot x

-- Simplificación de funciones inversas trigonométricas
simplifyASin :: Expr -> Expr
simplifyASin (Val n) 
    | n >= -1 && n <= 1 = Val (P.asin n)
    | otherwise = error "Dominio de arcoseno entre -1 y 1"
simplifyASin x = ASin x

simplifyACos :: Expr -> Expr
simplifyACos (Val n)
    | n >= -1 && n <= 1 = Val (P.acos n)
    | otherwise = error "Dominio de arcocoseno entre -1 y 1"
simplifyACos x = ACos x

simplifyATan :: Expr -> Expr
simplifyATan (Val n) = Val (P.atan n)
simplifyATan x = ATan x

simplifyACot :: Expr -> Expr
simplifyACot (Val n) = Val (P.pi / 2 - P.atan n)
simplifyACot x = ACot x

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

-- Derivadas de funciones trigonométricas
derivative (Sin u) var = Mul (Cos u) (derivative u var)
derivative (Cos u) var = Mul (Val (-1)) (Mul (Sin u) (derivative u var))
-- derivative (Tan u) var = Mul (Pow (Sec u) (Val 2)) (derivative u var)
-- derivative (Cot u) var = Mul (Val (-1)) (Mul (Pow (Csc u) (Val 2)) (derivative u var))

-- -- Funciones auxiliares para algunas derivadas
-- derivative (Sec u) var = Mul (Mul (Sec u) (Tan u)) (derivative u var)
-- derivative (Csc u) var = Mul (Val (-1)) (Mul (Csc u) (Cot u)) (derivative u var)

derivative _ _ = error "Derivada no soportada para esta expresión"

substituteVars :: Expr -> [(String, Double)] -> Expr
substituteVars expr vars = case expr of
    Val n -> Val n
    Var v -> case lookup v vars of
        Just val -> Val val
        Nothing  -> Var v
    Add x y -> Add (substituteVars x vars) (substituteVars y vars)
    Sub x y -> Sub (substituteVars x vars) (substituteVars y vars)
    Mul x y -> Mul (substituteVars x vars) (substituteVars y vars)
    Div x y -> Div (substituteVars x vars) (substituteVars y vars)
    Pow x y -> Pow (substituteVars x vars) (substituteVars y vars)
    Sqrt x -> Sqrt (substituteVars x vars)
    Sin x -> Sin (substituteVars x vars)
    Cos x -> Cos (substituteVars x vars)
    Tan x -> Tan (substituteVars x vars)
    Cot x -> Cot (substituteVars x vars)
    ASin x -> ASin (substituteVars x vars)
    ACos x -> ACos (substituteVars x vars)
    ATan x -> ATan (substituteVars x vars)
    ACot x -> ACot (substituteVars x vars)
    Deriv expr' var -> Deriv (substituteVars expr' vars) var
    Integrate expr' var -> Integrate (substituteVars expr' vars) var

evalDerivativeAtPoint :: Expr -> String -> [(String, Double)] -> Expr
evalDerivativeAtPoint expr var values = 
    let substitutedExpr = substituteVars expr values
        derivedExpr = derivative substitutedExpr var
    in eval derivedExpr

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
safeEval :: Expr -> IO (Either String Expr)
safeEval expr = do
    result <- Exception.try (evaluate (eval expr)) :: IO (Either SomeException Expr)
    return $ case result of
        Left ex  -> Left ("Error al evaluar la expresión: " ++ show ex)
        Right val -> Right val
        