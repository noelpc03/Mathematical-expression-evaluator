module Parser (Expr(..), parseExpr) where

-- Importaciones calificadas
import qualified Text.Parsec as Parsec
import qualified Control.Exception as Exception

-- Otras importaciones
import Text.Parsec.String (Parser)
import Text.Parsec (char, string, spaces, many1, digit, letter, choice, (<|>), chainl1)
import Control.Exception (SomeException, evaluate, try)

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
          | Deriv Expr String
          | Integrate Expr String
          deriving Show

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
                    Parsec.try (string "atan"), Parsec.try (string "acot")]
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

-- Parser para la raíz cuadrada
sqrtExpr :: Parser Expr
sqrtExpr = do
    string "sqrt"
    spaces
    expr <- parens <|> factor
    return $ Sqrt expr

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
    char ')'
    return $ Deriv expr var

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

-- Parser para factores (números, variables, paréntesis, raíz cuadrada, funciones trigonométricas, derivadas, integrales)
factor :: Parser Expr
factor = Parsec.try derivExpr <|> Parsec.try integrateExpr <|> Parsec.try trigFunc <|> Parsec.try sqrtExpr <|> Parsec.try number <|> Parsec.try variable <|> parens

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