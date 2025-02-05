module Parser (Expr(..), parseExpr) where

-- Importaciones calificadas
import qualified Text.Parsec as Parsec
import qualified Control.Exception as Exception

-- Otras importaciones
import Text.Parsec.String (Parser)
import Text.Parsec (char, string, spaces, many1, digit,optionMaybe, letter, choice, (<|>), chainl1,oneOf)
import Control.Exception (SomeException, evaluate, try)
import Text.Parsec.Char (digit)

-- Definimos un tipo de datos para representar expresiones aritméticas
data Expr = Val Double
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Sqrt Expr
          | Pow Expr Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | Cot Expr
          | ASin Expr
          | ACos Expr
          | ATan Expr
          | ACot Expr
          | Sec Expr
          | Csc Expr
          | Log Expr Expr
          | Ln Expr
          | Abs Expr
          | Exp Expr
          |Signum Expr
          | Deriv Expr String
          | DerivEval Expr String Double
          | Integrate Expr String
          | BinToDec Expr  -- Convierte binario a decimal
          | DecToBin Expr  -- Convierte decimal a binario
          | BinXor Expr Expr -- XOR binario
          | BinAnd Expr Expr -- AND binario
          | BinOr Expr Expr  -- OR binario
          | HexToDec Expr
          | DecToHex Expr
          | BinToHex Expr  
          | HexToBin Expr 
          deriving Show
---Parser para hexadecimal

-- -- Parser para el operador AND
-- andOp :: Parser (Expr -> Expr -> Expr)
-- andOp = do
--     string "and"
--     spaces
--     return And

-- -- Parser para el operador OR
-- orOp :: Parser (Expr -> Expr -> Expr)
-- orOp = do
--     string "or"
--     spaces
--     return Or

-- -- Parser para el operador XOR
-- xorOp :: Parser (Expr -> Expr -> Expr)
-- xorOp = do
--     string "xor"
--     spaces
--     return Xor

-- -- Parser para el operador NAND
-- nandOp :: Parser (Expr -> Expr -> Expr)
-- nandOp = do
--     string "nand"
--     spaces
--     return Nand


-- Parser para binario a hexadecimal
binToHexExpr :: Parser Expr
binToHexExpr = do
    string "bintohex"
    spaces
    expr <- many1 (oneOf "01")  -- Solo acepta 0 y 1
    return $ BinToHex (Val expr)

-- Parser para hexadecimal a binario
hexToBinExpr :: Parser Expr
hexToBinExpr = do
    string "hextobin"
    spaces
    expr <- hexNumber
    return $ HexToBin expr

-- Parser para números hexadecimales (Ejemplo: 0x1A3F)
hexNumber :: Parser Expr
hexNumber = do
    string "0x"  -- Prefijo hexadecimal
    hex <- many1 (oneOf "0123456789ABCDEFabcdef")
    return $ Val hex

-- Parser para convertir hexadecimal a decimal
hexToDecExpr :: Parser Expr
hexToDecExpr = do
    string "hextodec"
    spaces
    expr <- hexNumber
    return $ HexToDec expr

decimalNumber :: Parser Expr
decimalNumber = do
    digits <- many1 digit  -- Captura solo números decimales
    return $ Val digits

-- Parser para convertir decimal a hexadecimal
decToHexExpr :: Parser Expr
decToHexExpr = do
    string "dectohex"
    spaces
    expr <- decimalNumber
    return $ DecToHex expr



















-- Parser para números
number :: Parser Expr
number = do
    n <- many1 (digit <|> char '.')
    return $ Val (read n)

-- Parser para variables
variable :: Parser Expr
variable = do
    v <- many1 letter
    return $ Var v

-- Parser para expresiones entre paréntesis
parens :: Parser Expr
parens = do
    char '('
    expr <- parseExpr
    char ')'
    return expr

-- Parser para funciones trigonométricas
trigFunc :: Parser Expr
trigFunc = do
    func <- choice [Parsec.try (string "sin"), Parsec.try (string "cos"), Parsec.try (string "tan"),
                    Parsec.try (string "cot"), Parsec.try (string "asin"), Parsec.try (string "acos"),
                    Parsec.try (string "atan"), Parsec.try (string "acot"), Parsec.try (string "sec"),
                    Parsec.try (string "csc")]
    spaces
    expr <- parens <|> factor
    return $ case func of
        "sin"  -> Sin expr
        "cos"  -> Cos expr
        "tan"  -> Tan expr
        "cot"  -> Cot expr
        "asin" -> ASin expr
        "acos" -> ACos expr
        "atan" -> ATan expr
        "acot" -> ACot expr
        "sec"  -> Sec expr
        "csc" -> Csc expr

-- Parser para la raíz cuadrada
sqrtExpr :: Parser Expr
sqrtExpr = do
    string "sqrt"
    spaces
    expr <- parens <|> factor
    return $ Sqrt expr

-- Parser para valor absoluto
absFunc :: Parser Expr
absFunc = do
    string "abs"
    spaces
    char '('
    expr <- parseExpr
    char ')'
    return $ Abs expr

-- Parser para el signum
signumFunc :: Parser Expr
signumFunc = do
    string "signum"
    spaces
    char '('
    expr <- parseExpr
    char ')'
    return $ Signum expr

-- Parser para derivadas
derivExpr :: Parser Expr
derivExpr = do
    string "deriv"
    spaces
    char '('
    expr <- parseExpr
    char ','
    spaces
    var <- many1 letter
    -- Intenta parsear un tercer parámetro opcional
    maybePoint <- optionMaybe (do
        char ','
        spaces
        point <- many1 (digit <|> char '.')
        return (read point))
    char ')'
    return $ case maybePoint of
        Nothing -> Deriv expr var
        Just point -> DerivEval expr var point

-- Parser para funciones exponencial y logarítmica
expFunc :: Parser Expr
expFunc = do
    string "exp"
    spaces
    expr <- parens <|> factor
    return $ Exp expr

logFunc :: Parser Expr
logFunc = do
    string "log"
    spaces
    char '('
    base <- parseExpr
    char ','
    spaces
    arg <- parseExpr
    char ')'
    return $ Log base arg

lnFunc :: Parser Expr
lnFunc = do
    string "ln"
    spaces
    char '('
    expr <- parseExpr
    char ')'
    return $ Ln expr

-- Parser para integrales
integrateExpr :: Parser Expr
integrateExpr = do
    string "integrate"
    spaces
    char '('
    expr <- parseExpr
    char ','
    spaces
    var <- many1 letter
    char ')'
    return $ Integrate expr var

-- Parser para operaciones binarias
binToDecExpr :: Parser Expr
binToDecExpr = do
    string "bintodec"
    spaces
    expr <- parens <|> factor
    return $ BinToDec expr


decToBinExpr :: Parser Expr
decToBinExpr = do
    string "dectobin"
    spaces
    expr <- parens <|> factor
    return $ DecToBin expr

binXorExpr :: Parser Expr
binXorExpr = do
    string "xor"
    spaces
    char '('
    expr1 <- parseExpr
    char ','
    spaces
    expr2 <- parseExpr
    char ')'
    return $ BinXor expr1 expr2

binAndExpr :: Parser Expr
binAndExpr = do
    string "and"
    spaces
    char '('
    expr1 <- parseExpr
    char ','
    spaces
    expr2 <- parseExpr
    char ')'
    return $ BinAnd expr1 expr2

binOrExpr :: Parser Expr
binOrExpr = do
    string "or"
    spaces
    char '('
    expr1 <- parseExpr
    char ','
    spaces
    expr2 <- parseExpr
    char ')'
    return $ BinOr expr1 expr2



-- Parser para factores (números, variables, paréntesis, raíz cuadrada, funciones trigonométricas, derivadas, integrales)
factor :: Parser Expr
factor = Parsec.try signumFunc 
    <|>Parsec.try absFunc   
    <|>Parsec.try derivExpr 
    <|> Parsec.try integrateExpr 
    <|> Parsec.try trigFunc 
    <|> Parsec.try sqrtExpr 
    <|> Parsec.try logFunc 
    <|> Parsec.try lnFunc 
    <|> Parsec.try number 
    <|> Parsec.try variable 
    <|> Parsec.try expFunc 
    <|>Parsec.try binToDecExpr
    <|> Parsec.try decToBinExpr
    <|> Parsec.try binXorExpr
    <|> Parsec.try binAndExpr
    <|> Parsec.try binOrExpr
    <|> Parsec.try binToHexExpr
    <|> Parsec.try hexToBinExpr
    <|> Parsec.try hexToDecExpr
    <|> Parsec.try decToHexExpr
    <|> Parsec.try hexNumber
    <|> parens
    

-- Parser para potencias
power :: Parser Expr
power = chainl1 factor powOp

-- Parser para términos (multiplicación y división)
term :: Parser Expr
term = chainl1 power (mulOp <|> divOp)

-- Parser para expresiones (suma y resta)
parseExpr :: Parser Expr
parseExpr = chainl1 term (addOp <|> subOp)

-- Operadores
addOp, subOp, mulOp, divOp, powOp :: Parser (Expr -> Expr -> Expr)
addOp = do { char '+'; return Add }
subOp = do { char '-'; return Sub }
mulOp = do { char '*' ; return Mul }
divOp = do { char '/' ; return Div }
powOp = do { char '^' ; return Pow }

