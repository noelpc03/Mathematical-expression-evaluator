module Evaluator (eval, evalBinary, safeEval, derivative, exprToString) where

import Parser (Expr(..))
import Numeric (showIntAtBase,readHex, showHex)
import Data.Char (intToDigit)
import Prelude hiding (tan, cos, sin, signum, log, exp)
import qualified Prelude as P
import qualified Control.Exception as Exception
import Control.Exception (SomeException, evaluate)
import Binary (binaryToDecimal, decimalToBinary, evalBinOp) 
import Data.Bits (shiftL, shiftR, (.&.)) 

-- Función para evaluar una expresión y simplificarla
eval :: Expr -> Expr
eval (Val n)     = Val n
eval (Var v)
    | v == "e" = Val (P.exp 1)
    | otherwise = Var v
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
eval (Csc x)     = simplifyCsc (eval x)
eval (Sec x)     = simplifySec (eval x)
eval (Sec x) = 
    case eval x of
        Val n -> Val (1 / P.cos n)
        expr -> Sec expr
eval (Csc x) = 
    case eval x of
        Val n -> Val (1 / P.sin n)
        expr -> Csc expr
eval (Exp x) = Exp (eval x)
eval (Ln x) = 
    case eval x of
        Val n 
            | n > 0     -> Val (P.log n)
            | otherwise -> error "Logaritmo natural solo definido para valores positivos"
        expr -> Ln expr

eval (Log base arg) = 
    case (eval base, eval arg) of
        (Val baseVal, Val argVal) 
            | baseVal > 0 && baseVal /= 1 && argVal > 0 -> 
                Val (P.logBase baseVal argVal)
            | isEBase base -> 
                Ln arg  -- Base e, convierte a logaritmo natural
        (_, _) -> Log (eval base) (eval arg)

eval (Deriv expr var) = eval (derivative expr var)
eval (DerivEval expr var point) = 
    let derivedExpr = derivative expr var
        substitutedExpr = substituteVars derivedExpr [(var, point)]
    in eval substitutedExpr
eval (Integrate expr var) = eval (integral expr var)

eval (Abs x) = 
    case eval x of
        Val n -> Val (abs n)
        Sub (Val 0) y -> eval y  -- Caso especial para -6
        _ -> Abs (eval x)

eval (Signum x) = signum (eval x)



-- Función separada para operaciones binarias
evalBinary :: Expr -> String
evalBinary (DecToBin (Val n)) = reverse (decimalToBinary (round n))
evalBinary (BinToDec (Val n)) = show (binaryToDecimal (show (round n)))
evalBinary (BinXor (Val x) (Val y)) = reverse (evalBinOp "xor" (show (round x)) (show (round y)))
evalBinary (BinAnd (Val x) (Val y)) = reverse (evalBinOp "and" (show (round x)) (show (round y)))
evalBinary (BinOr  (Val x) (Val y)) = reverse (evalBinOp "or" (show (round x)) (show (round y)))
evalBinary (BinNand (Val x) (Val y)) = reverse (evalBinOp "nand" (show (round x)) (show (round y)))
evalBinary _ = "Operación no válida"


instance Eq Expr where
    (Val a) == (Val b) = a == b
    (Var a) == (Var b) = a == b
    (Add x1 y1) == (Add x2 y2) = x1 == x2 && y1 == y2
    (Sub x1 y1) == (Sub x2 y2) = x1 == x2 && y1 == y2
    (Mul x1 y1) == (Mul x2 y2) = x1 == x2 && y1 == y2
    (Div x1 y1) == (Div x2 y2) = x1 == x2 && y1 == y2
    (Pow x1 y1) == (Pow x2 y2) = x1 == x2 && y1 == y2
    (Sin x1) == (Sin x2) = x1 == x2
    (Cos x1) == (Cos x2) = x1 == x2
    (Ln x1) == (Ln x2) = x1 == x2
    (Log base1 arg1) == (Log base2 arg2) = base1 == base2 && arg1 == arg2
    _ == _ = False


-- **Función para convertir una expresión a una cadena de texto**
exprToString :: Expr -> String
exprToString (Val n) = show n
exprToString (Var v) = v
exprToString (Add x y) = "(" ++ exprToString x ++ " + " ++ exprToString y ++ ")"
exprToString (Sub x y) = "(" ++ exprToString x ++ " - " ++ exprToString y ++ ")"
exprToString (Mul x y) = exprToString x ++ " * " ++ exprToString y
exprToString (Mul (Var v) (Val a)) = v ++ " * " ++ show a
exprToString (Mul x y) = exprToString x ++ " * " ++ exprToString y

exprToString (Div x y) = exprToString x ++ " / " ++ exprToString y
exprToString (Sqrt x) = "sqrt(" ++ exprToString x ++ ")"
exprToString (Pow x y) = exprToString x ++ " ^ " ++ exprToString y
exprToString (Exp x) = "exp(" ++ exprToString x ++ ")"
exprToString (Signum x) = "signum(" ++ exprToString x ++ ")"
exprToString (Abs x) = "|" ++ exprToString x ++ "|"
exprToString (Ln x) = "ln(" ++ exprToString x ++ ")"
exprToString (Log base arg) = 
    case (base, arg) of
        (Val baseVal, _) 
            | isEBase base -> 
                "ln(" ++ exprToString arg ++ ")"
            | otherwise -> 
                "log_" ++ exprToString base ++ "(" ++ exprToString arg ++ ")"
        _ -> "log_" ++ exprToString base ++ "(" ++ exprToString arg ++ ")"

exprToString (Val n) 
    | n == fromInteger (round n) = show (round n) ++ ".0"
    | otherwise = show n


exprToString (Signum (Val n)) = 
    "signum(" ++ 
    (if n > 0 then "1.0" else 
    if n < 0 then "-1.0" else "0.0") ++ 
    ")"
exprToString (Sqrt (Val n)) = "sqrt(" ++ show n ++ ")"
exprToString (Abs (Val n)) = "abs(" ++ show (abs n) ++ ")"
exprToString (Pow (Var n) (Val e)) = "pow(" ++ show n ++ "," ++ show e ++ ")"
exprToString (Abs (Val n)) = "|" ++ show (abs n) ++ "|"


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
exprToString (Sec x) = "sec(" ++ exprToString x ++ ")"
exprToString (Csc x) = "csc(" ++ exprToString x ++ ")"

-- operaciones complejas
exprToString (Integrate expr var) = "integrate(" ++ exprToString expr ++ ", " ++ var ++ ")"
exprToString (Deriv expr var) = 
    "deriv(" ++ exprToString expr ++ ", " ++ var ++ ")"
exprToString (DerivEval expr var point) = 
    "deriv(" ++ exprToString expr ++ ", " ++ var ++ ", " ++ show point ++ ")"

-- 
exprToString x = error $ "Expresión no soportada en exprToString: " ++ show x

signum :: Expr -> Expr
signum (Val n)
    | n > 0     = Val 1.0
    | n < 0     = Val (-1.0)
    | otherwise = Val 0.0
signum expr = Signum expr


-- **Simplificaciones**
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

-- Simplificación de valor absoluto
simplifyAbs :: Expr -> Expr
simplifyAbs (Val n) = Val (abs n)
simplifyAbs (Mul (Val n) x) 
    | n < 0     = Mul (Val (abs n)) x
    | otherwise = Abs (Mul (Val n) x)
simplifyAbs x = Abs x

-- Simplificación de logaritmo natural
simplifyLn :: Expr -> Expr
simplifyLn (Val n)
    | n > 0     = Val (P.log n)
    | otherwise = error "Logaritmo natural solo definido para valores positivos"
simplifyLn x = Ln x

-- Simplificación de logaritmo de base arbitraria
simplifyLog :: Expr -> Expr -> Expr
simplifyLog base arg
    | isEBase base = Ln arg  -- base e
    | otherwise = Log base arg

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

-- Simplificación de cosecante
simplifyCsc :: Expr -> Expr
simplifyCsc (Val n) = Val (1 / P.sin n)
simplifyCsc x = Csc x

-- Simplificación de secante
simplifySec :: Expr -> Expr
simplifySec (Val n) = Val (1 / P.cos n)
simplifySec x = Sec x


-- **Derivada simbólica**
derivative :: Expr -> String -> Expr
derivative (Val _) _ = Val 0
derivative (Var v) var
    | v == var  = Val 1
    | otherwise = Val 0

-- regla de derivacion de la suma
derivative (Add u v) var = 
    let du = derivative u var
        dv = derivative v var
    in case (du, dv) of
        (Val 0, dv') -> dv'
        (du', Val 0) -> du'
        _ -> Add du dv

--regla de derivacion de la resta
derivative (Sub u v) var = 
    let du = derivative u var
        dv = derivative v var
    in case (du, dv) of
        (Val 0, dv') -> dv'
        (du', Val 0) -> du'
        _ -> Sub du dv

-- Regla del producto
derivative (Mul u v) var =
    Add 
        (Mul (derivative u var) v) 
        (Mul u (derivative v var))

-- Regla del cociente
derivative (Div u v) var = 
    Div 
        (Sub 
            (Mul (derivative u var) v) 
            (Mul u (derivative v var))) 
        (Pow v (Val 2))

-- Regla de la potencia
derivative (Pow u (Val n)) var = 
    Mul 
        (Mul (Val n) (Pow u (Val (n - 1)))) 
        (derivative u var)

-- Derivada de raíz cuadrada
derivative (Sqrt u) var = 
    Div (derivative u var) (Mul (Val 2.0) (Sqrt u))

-- Logaritmo, logaritmo neperiano y exponencial
derivative (Exp u) var = 
    Mul (Exp u) (derivative u var)
derivative (Ln u) var = 
    Div (derivative u var) u
derivative (Log base u) var =
    Div 
        (derivative u var) 
        (Mul u (Ln base))


-- Derivada de valor absoluto
derivative (Abs u) var = 
    Mul (Signum u) (derivative u var)

--Derivada de un signum
derivative (Signum u) var = 
    Val 0.0  -- La derivada de signum es 0 en casi todos los puntos


-- Regla de la cadena para funciones trigonométricas
derivative (Sin u) var = 
    -- d/dx sin(u) = cos(u) * du/dx
    Mul (Cos u) (derivative u var)
derivative (Cos u) var = 
    -- d/dx cos(u) = -sin(u) * du/dx
    Mul (Val (-1)) (Mul (Sin u) (derivative u var))
derivative (Tan u) var = 
    -- d/dx tan(u) = sec²(u) * du/dx
    Div (derivative u var) (Pow (Cos u) (Val 2))
derivative (Cot u) var = 
    Mul (Val (-1)) (Div (derivative u var) (Pow (Sin u) (Val 2)))
    -- d/dx(sec(x)) = sec(x) * tan(x)
derivative (Sec u) var = 
    Mul (Mul (Sec u) (Tan u)) (derivative u var)
    -- d/dx(csc(x)) = -csc(x) * cot(x)
derivative (Csc u) var = 
    let dcscTerm = Mul (Csc u) (Cot u)
        du = derivative u var
    in Mul (Val (-1)) (Mul dcscTerm du)


-- Derivadas de funciones inversas trigonométricas
-- Derivada de arcoseno (asin) d/dx(arcsin(x)) = 1 / √(1 - x²)
derivative (ASin u) var = 
    Div (derivative u var) 
        (Sqrt (Sub (Val 1) (Pow u (Val 2))))

-- Derivada de arcocoseno (acos) d/dx(arccos(x)) = -1 / √(1 - x²)
derivative (ACos u) var = 
    Mul (Val (-1)) 
        (Div (derivative u var) 
            (Sqrt (Sub (Val 1) (Pow u (Val 2)))))

-- Derivada de arcotangente (atan) d/dx(arctan(x)) = 1 / (1 + x²)
derivative (ATan u) var = 
    Div (derivative u var) 
        (Add (Val 1) (Pow u (Val 2)))

-- Derivada de arcocotangente (acot) d/dx(arccot(x)) = -1 / (1 + x²)
derivative (ACot u) var = 
    Mul (Val (-1)) 
        (Div (derivative u var) 
            (Add (Val 1) (Pow u (Val 2))))

-- Casos generales para funciones compuestas
derivative expr@(Sin (Pow u _)) var = 
    Mul (Cos expr) (derivative u var)
derivative expr@(Cos (Pow u _)) var = 
    Mul (Val (-1)) (Mul (Sin expr) (derivative u var))
derivative expr@(Tan (Pow u _)) var = 
    Div (derivative u var) (Pow (Cos expr) (Val 2))

-- Caso recursivo para composición de funciones
derivative expr@(Mul u v) var 
    | containsVar var u && containsVar var v = 
        Add 
            (Mul (derivative u var) v) 
            (Mul u (derivative v var))
    | containsVar var u = 
        Mul (derivative u var) v
    | containsVar var v = 
        Mul u (derivative v var)
    | otherwise = Val 0

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
    Sec x -> Sec (substituteVars x vars)
    Csc x -> Csc (substituteVars x vars)
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
    | v == var  = Div (Pow (Var v) (Val 2)) (Val 2)  -- ∫x dx = x²/2
    | otherwise = Mul (Var v) (Var var)  -- ∫y dx = y*x

-- Integral de suma
integral (Add u v) var = 
    Add (integral u var) (integral v var)

-- Integral de resta
integral (Sub u v) var = 
    Sub (integral u var) (integral v var)

-- Integral de multiplicación
integral (Mul u v) var
    | not (containsVar var u) = 
        Mul u (integral v var)
    | not (containsVar var v) = 
        Mul v (integral u var)
    | otherwise = error "Integral de producto complejo no soportada"

-- Integral del cociente (división)
integral (Div u v) var
    | containsVar var u && not (containsVar var v) = 
        case v of
            Val n -> 
                Div (Pow u (Val 2)) (Mul (Val 2) (Val n))  -- ∫(x/c)dx = x²/(2*c)
            _ -> Mul u (Ln (Abs v))
    | not (containsVar var u) && containsVar var v = 
        Mul (u) (Ln (Abs v))
    | otherwise = 
        Mul (Div u v) (Var var)

--Integral de valor absoluto
integral (Abs u) var
    | containsVar var u = 
        Mul (Signum u) (Div (Pow u (Val 2)) (Val 2))
    | otherwise = Mul (Abs u) (Var var)

-- Integral de la potencia
integral (Pow u (Val n)) var
    | n /= (-1) = 
        Div 
            (Pow u (Val (n + 1))) 
            (Val (n + 1))
    | n == (-1) = Ln (Abs u)  -- ∫ 1/x dx = ln(|x|)
    | otherwise = error "Integral de potencia no soportada"

-- Integral de la raíz cuadrada
integral (Sqrt u) var
    | containsVar var u = 
        Div 
            (Mul (Val 2.0) (Pow u (Val (3.0/2.0)))) 
            (Val 3.0)  -- ∫√x dx = (2*x^(3/2))/3
    | otherwise = 
        Mul (Sqrt u) (Var var)

-- Integral Logaritmo, Logaritmo neperiano y exponencial
integral (Exp u) var 
    | containsVar var u = Exp u
    | otherwise = Mul (Exp u) (Var var)

integral (Ln u) var
    | containsVar var u = 
        Sub 
            (Mul u (Ln u)) 
            u
    | otherwise = 
        Mul (Ln u) (Var var)

integral (Log base u) var
    | containsVar var u = 
        Div 
            (Sub 
                (Mul u (Ln u)) 
                u) 
            (Ln base)
    | otherwise = 
        Mul (Log base u) (Var var)  

-- Integral de un signum
integral (Signum u) var
    | containsVar var u = Val 0.0
    | otherwise = Mul (Signum u) (Var var)

-- Integrales de funciones trigonométricas
integral (Sin u) var
    | containsVar var u = 
        Mul (Val (-1)) (Cos u)
    | otherwise = error "Integral de seno no soportada"

integral (Cos u) var
    | containsVar var u = 
        Sin u
    | otherwise = error "Integral de coseno no soportada"

integral (Tan u) var
    | containsVar var u = Mul (Val (-1)) (Ln (Abs (Cos u)))
    | otherwise = Mul (Tan u) (Var var)

integral (Cot u) var
    | containsVar var u = 
        Ln (Abs (Sin u))  -- ∫cot(x) dx = ln|sin(x)|
    | otherwise = 
        Mul (Cot u) (Var var)

-- ∫sec(x) dx = ln|sec(x) + tan(x)|
integral (Sec u) var
    | containsVar var u = 
        Ln (Abs (Add (Sec u) (Tan u))) 
    | otherwise = 
        Mul (Sec u) (Var var)

-- ∫csc(x) dx = ln|csc(x) + cot(x)|
integral (Csc u) var
    | containsVar var u = 
        Ln (Abs (Add (Csc u) (Cot u)))
    | otherwise = 
        Mul (Csc u) (Var var)


-- Integral de las funciones inversas trigonometricas
-- Integral de arcoseno (asin) ∫arcsin(x)dx = x * arcsin(x) + √(1 - x²)
integral (ASin u) var
    | containsVar var u = 
        let x = u
        in Sub 
            (Mul x (ASin x)) 
            (Sqrt (Sub (Val 1) (Pow x (Val 2))))

-- Integral de arcocoseno (acos) ∫arccos(x)dx = x * arccos(x) - √(1 - x²)
integral (ACos u) var
    | containsVar var u = 
        let x = u
        in Add 
            (Mul x (ACos x)) 
            (Sqrt (Sub (Val 1) (Pow x (Val 2))))

-- Integral de arcotangente (atan) ∫arctan(x)dx = x * arctan(x) - (1/2) * ln(1 + x²)
integral (ATan u) var
    | containsVar var u = 
        let x = u
        in Sub 
            (Mul x (ATan x)) 
            (Div (Ln (Add (Val 1) (Pow x (Val 2)))) (Val 2))

-- Integral de arcocotangente (acot) ∫arccot(x)dx = x * arccot(x) + (1/2) * ln(1 + x²)
integral (ACot u) var
    | containsVar var u = 
        let x = u
        in Add 
            (Mul x (ACot x)) 
            (Div (Ln (Add (Val 1) (Pow x (Val 2)))) (Val 2))

-- Caso por defecto
integral _ _ = error "Integral no soportada para esta expresión"


-- Evaluación segura que captura excepciones
safeEval :: Expr -> IO (Either String Expr)
safeEval expr = do
    case expr of
        DecToBin _ -> return $ Left (evalBinary expr)  -- Devuelve binario como String
        BinToDec _ -> return $ Left (evalBinary expr)  -- También devuelve String
        BinXor _ _ -> return $ Left (evalBinary expr)
        BinAnd _ _ -> return $ Left (evalBinary expr)
        BinOr _ _  -> return $ Left (evalBinary expr)
        BinNand _ _ -> return $ Left (evalBinary expr)
        DecToHex _ -> return $ Left (evalHex expr)
        HexToBin _ -> return $ Left (evalHex expr)
        HexToDec _ -> return $ Left (evalHex expr)
        BinToHex _ -> return $ Left (evalHex expr)
        _ -> do
            result <- Exception.try (evaluate (eval expr)) :: IO (Either SomeException Expr)
            return $ case result of
                Left ex -> Left ("Error al evaluar la expresión: " ++ show ex)
                Right val -> Right val

-- Función auxiliar para identificar base e
isEBase :: Expr -> Bool
isEBase (Val n) = abs (n - P.exp 1) < 1e-10
isEBase _ = False

-- Función auxiliar para verificar si una variable está presente
containsVar :: String -> Expr -> Bool
containsVar var (Var v) = v == var
containsVar var (Val _) = False
containsVar var (Add u v) = containsVar var u || containsVar var v
containsVar var (Sub u v) = containsVar var u || containsVar var v
containsVar var (Mul u v) = containsVar var u || containsVar var v
containsVar var (Div u v) = containsVar var u || containsVar var v
containsVar var (Pow u _) = containsVar var u
containsVar var (Sin u) = containsVar var u
containsVar var (Cos u) = containsVar var u
containsVar var (Tan u) = containsVar var u
containsVar var (Cot u) = containsVar var u
containsVar var (ASin u) = containsVar var u
containsVar var (ACos u) = containsVar var u
containsVar var (ATan u) = containsVar var u
containsVar var (ACot u) = containsVar var u
containsVar var (Exp u) = containsVar var u
containsVar var (Ln u) = containsVar var u
containsVar var (Log base u) = 
    containsVar var base || containsVar var u
containsVar var (Abs u) = containsVar var u
containsVar var (Deriv u _) = containsVar var u
containsVar var (Integrate u _) = containsVar var u
containsVar _ _ = False




binToDecimal :: String -> Int 
binToDecimal = foldl (\acc bit -> acc * 2 + if bit == '1' then 1 else 0) 0 

decimalToBinario :: Int -> String 
decimalToBinario 0 = "0" 
decimalToBinario n = reverse (helper n) 
    where 
        helper 0 = "" 
        helper x = intToDigit (x `mod` 2) : helper (x `div` 2) 
        
hexToBinary :: String -> String 
hexToBinary hex = decimalToBinario (hexToDecimal hex) 

binaryToHex :: String -> String 
binaryToHex bin = decimalToHex (binToDecimal bin) 

hexToDecimal :: String -> Int 
hexToDecimal hexStr = 
    case readHex hexStr of 
        [(val, "")] -> val 
        _           -> error "Formato hexadecimal inválido" 

-- Convierte un entero a string hexadecimal 
decimalToHex :: Int -> String 
decimalToHex n = "0x" ++ showHex n "" 

-- Evalúa una expresión hexadecimal 
evalHex :: Expr -> String 
evalHex (Var hex) = hex 
evalHex (Add x y) = decimalToHex $ (hexToDecimal (evalHex x) + hexToDecimal (evalHex y) )
evalHex (Sub x y) = decimalToHex $ (hexToDecimal (evalHex x) - hexToDecimal (evalHex y) )
evalHex (Mul x y) = decimalToHex $ (hexToDecimal (evalHex x) * hexToDecimal (evalHex y) )
evalHex (Div x y) = decimalToHex $ (hexToDecimal (evalHex x) `div` hexToDecimal (evalHex y) )
evalHex (HexToDec x) = show $ hexToDecimal (evalHex x) 
evalHex (DecToHex (Var x)) = decimalToHex (read x) 
evalHex (BinToHex (Var bin)) = binaryToHex bin 
evalHex (HexToBin (Var hex)) = hexToBinary hex 
evalHex _ = "Operación no válida"


