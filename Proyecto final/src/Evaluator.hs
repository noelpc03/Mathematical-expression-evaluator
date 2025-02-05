module Evaluator (evalMath, evalBinary, safeEval, derivative) where

import Parser (Expr(..))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Prelude hiding (tan, cos, sin)
import qualified Prelude as P
import qualified Control.Exception as Exception
import Control.Exception (SomeException, evaluate)
import Binary (binaryToDecimal, decimalToBinary, evalBinOp) -- Importamos Binary

-- Función para evaluar una expresión
evalMath :: Expr -> Double
evalMath (Val n)     = n
evalMath (Add x y)   = evalMath x + evalMath y
evalMath (Sub x y)   = evalMath x - evalMath y
evalMath (Mul x y)   = evalMath x * evalMath y
evalMath (Div x y)   = evalMath x / evalMath y
evalMath (Sqrt x)    = sqrt (evalMath x)
evalMath (Pow x y)   = evalMath x ** evalMath y
evalMath (Sin x)     = P.sin (evalMath x)
evalMath (Cos x)     = P.cos (evalMath x)
evalMath (Tan x)     = P.tan (evalMath x)
evalMath (Cot x)     = 1 / P.tan (evalMath x)
evalMath (ASin x)    = P.asin (evalMath x)
evalMath (ACos x)    = P.acos (evalMath x)
evalMath (ATan x)    = P.atan (evalMath x)
evalMath (ACot x)    = P.pi / 2 - P.atan (evalMath x)
evalMath (Deriv expr var) = evalMath (derivative expr var)
evalMath (Integrate expr var) = evalMath (integral expr var)



-- Función separada para operaciones binarias
evalBinary :: Expr -> String
evalBinary (DecToBin (Val n)) = reverse (decimalToBinary (round n))
evalBinary (BinToDec (Val n)) = show (binaryToDecimal (show (round n)))
evalBinary (BinXor (Val x) (Val y)) = reverse (evalBinOp "xor" (show (round x)) (show (round y)))
evalBinary (BinAnd (Val x) (Val y)) = reverse (evalBinOp "and" (show (round x)) (show (round y)))
evalBinary (BinOr  (Val x) (Val y)) = reverse (evalBinOp "or" (show (round x)) (show (round y)))
evalBinary _ = "Operación no válida"




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
safeEval :: Expr -> IO (Either String (Either String Double))
safeEval expr = do
    case expr of
        DecToBin _ -> return $ Left (evalBinary expr)  -- Devuelve binario como String
        BinToDec _ -> return $ Left (evalBinary expr)  -- También devuelve String
        BinXor _ _ -> return $ Left (evalBinary expr)
        BinAnd _ _ -> return $ Left (evalBinary expr)
        BinOr _ _  -> return $ Left (evalBinary expr)
        _ -> do
            result <- Exception.try (evaluate (evalMath expr)) :: IO (Either SomeException Double)
            return $ case result of
                Left ex  -> Right (Left ("Error al evaluar la expresión: " ++ show ex))
                Right val -> Right (Right val)
