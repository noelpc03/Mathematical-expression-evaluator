module HexParser (HexExpr(..), parseHexExpr) where

-- Importaciones calificadas
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec (char, string, spaces, many1, digit,oneOf, choice, (<|>), chainl1)

-- Definimos un tipo de datos para representar expresiones hexadecimales
data HexExpr = Val Double
             | Add HexExpr HexExpr
             | Sub HexExpr HexExpr
             | Mul HexExpr HexExpr
             | Div HexExpr HexExpr
             | Sqrt HexExpr
             | Pow HexExpr HexExpr
             | HexToDec HexExpr
             | DecToHex HexExpr
             | HexXor HexExpr HexExpr
             | HexAnd HexExpr HexExpr
             | HexOr HexExpr HexExpr
             | BinToHex HexExpr
             | HexToBin HexExpr
             deriving Show

-- Parser para números hexadecimales
hexNumber :: Parser HexExpr
hexNumber = do
    string "0x"
    hex <- many1 (oneOf "0123456789ABCDEFabcdef")
    return $ Val (hexToDecimal hex)

-- Función para convertir de hexadecimal a decimal
hexToDecimal :: String -> Double
hexToDecimal hex = fromIntegral (read ("0x" ++ hex) :: Integer)

-- Parser para expresiones entre paréntesis
parens :: Parser HexExpr
parens = do
    char '('
    expr <- parseHexExpr
    char ')'
    return expr

-- Parser para la raíz cuadrada
sqrtExpr :: Parser HexExpr
sqrtExpr = do
    string "sqrt"
    spaces
    expr <- parens <|> factor
    return $ Sqrt expr

-- Parser para operaciones hexadecimales
hexToDecExpr :: Parser HexExpr
hexToDecExpr = do
    string "hextodec"
    spaces
    expr <- parens <|> hexNumber
    return $ HexToDec expr

-- Parser para números decimales
decimalNumber :: Parser HexExpr
decimalNumber = do
    digits <- many1 digit  -- Toma uno o más dígitos
    return $ Val (read digits)  -- Convierte el número decimal a un valor


decToHexExpr :: Parser HexExpr
decToHexExpr = do
    string "dectohex"
    spaces
    expr <- decimalNumber
    return $ DecToHex expr

-- Parser para relacion con numeros binarios

binToHexExpr :: Parser HexExpr
binToHexExpr = do
    string "bintohex"
    spaces
    expr <- parens <|> hexNumber
    return $ BinToHex expr

hexToBinExpr :: Parser HexExpr
hexToBinExpr = do
    string "hextobin"
    spaces
    expr <- parens <|> hexNumber
    return $ HexToBin expr



hexXorExpr :: Parser HexExpr
hexXorExpr = do
    string "xor"
    spaces
    char '('
    expr1 <- parseHexExpr
    char ','
    spaces
    expr2 <- parseHexExpr
    char ')'
    return $ HexXor expr1 expr2




hexAndExpr :: Parser HexExpr
hexAndExpr = do
    string "and"
    spaces
    char '('
    expr1 <- parseHexExpr
    char ','
    spaces
    expr2 <- parseHexExpr
    char ')'
    return $ HexAnd expr1 expr2

hexOrExpr :: Parser HexExpr
hexOrExpr = do
    string "or"
    spaces
    char '('
    expr1 <- parseHexExpr
    char ','
    spaces
    expr2 <- parseHexExpr
    char ')'
    return $ HexOr expr1 expr2

-- Parser para factores
factor :: Parser HexExpr
factor = Parsec.try hexToDecExpr
     <|> Parsec.try decToHexExpr
     <|> Parsec.try hexXorExpr
     <|> Parsec.try hexAndExpr
     <|> Parsec.try hexOrExpr
     <|> Parsec.try sqrtExpr
     <|> Parsec.try hexNumber
     <|> parens

-- Parser para potencias
power :: Parser HexExpr
power = chainl1 factor powOp

-- Parser para términos (multiplicación y división)
term :: Parser HexExpr
term = chainl1 power (mulOp <|> divOp)

-- Parser para expresiones (suma y resta)
parseHexExpr :: Parser HexExpr
parseHexExpr = chainl1 term (addOp <|> subOp)

-- Operadores
addOp, subOp, mulOp, divOp, powOp :: Parser (HexExpr -> HexExpr -> HexExpr)
addOp = do { char '+'; return Add }
subOp = do { char '-'; return Sub }
mulOp = do { char '*'; return Mul }
divOp = do { char '/'; return Div }
powOp = do { char '^'; return Pow }