module HexParser (HexExpr(..), parseHexExpr, parseHexString) where

import Text.Parsec (parse, (<|>), many1, chainl1, try, char, string, spaces, oneOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit)

-- Tipo de datos para representar expresiones hexadecimales
data HexExpr = Val String
             | Add HexExpr HexExpr
             | Sub HexExpr HexExpr
             | Mul HexExpr HexExpr
             | Div HexExpr HexExpr
             | HexToDec HexExpr
             | DecToHex HexExpr
             | BinToHex HexExpr  
             | HexToBin HexExpr  
             | And HexExpr HexExpr  
             | Or HexExpr HexExpr   
             | Xor HexExpr HexExpr  
             | Nand HexExpr HexExpr 
             deriving (Show)

-- Parser para el operador AND
andOp :: Parser (HexExpr -> HexExpr -> HexExpr)
andOp = do
    string "and"
    spaces
    return And

-- Parser para el operador OR
orOp :: Parser (HexExpr -> HexExpr -> HexExpr)
orOp = do
    string "or"
    spaces
    return Or

-- Parser para el operador XOR
xorOp :: Parser (HexExpr -> HexExpr -> HexExpr)
xorOp = do
    string "xor"
    spaces
    return Xor

-- Parser para el operador NAND
nandOp :: Parser (HexExpr -> HexExpr -> HexExpr)
nandOp = do
    string "nand"
    spaces
    return Nand


-- Parser para binario a hexadecimal
binToHexExpr :: Parser HexExpr
binToHexExpr = do
    string "bintohex"
    spaces
    expr <- many1 (oneOf "01")  -- Solo acepta 0 y 1
    return $ BinToHex (Val expr)

-- Parser para hexadecimal a binario
hexToBinExpr :: Parser HexExpr
hexToBinExpr = do
    string "hextobin"
    spaces
    expr <- hexNumber
    return $ HexToBin expr

-- Parser para números hexadecimales (Ejemplo: 0x1A3F)
hexNumber :: Parser HexExpr
hexNumber = do
    string "0x"  -- Prefijo hexadecimal
    hex <- many1 (oneOf "0123456789ABCDEFabcdef")
    return $ Val hex

-- Parser para convertir hexadecimal a decimal
hexToDecExpr :: Parser HexExpr
hexToDecExpr = do
    string "hextodec"
    spaces
    expr <- hexNumber
    return $ HexToDec expr

decimalNumber :: Parser HexExpr
decimalNumber = do
    digits <- many1 digit  -- Captura solo números decimales
    return $ Val digits

-- Parser para convertir decimal a hexadecimal
decToHexExpr :: Parser HexExpr
decToHexExpr = do
    string "dectohex"
    spaces
    expr <- decimalNumber
    return $ DecToHex expr

-- Parser para expresiones entre paréntesis
parens :: Parser HexExpr
parens = do
    char '('
    expr <- parseHexExpr
    char ')'
    return expr

-- Operadores aritméticos básicos (+, -, *, /)
addOp, subOp, mulOp, divOp :: Parser (HexExpr -> HexExpr -> HexExpr)
addOp = do { char '+'; return Add }
subOp = do { char '-'; return Sub }
mulOp = do { char '*'; return Mul }
divOp = do { char '/'; return Div }

-- Operadores lógicos (and, or, xor, nand)
logicalOp :: Parser (HexExpr -> HexExpr -> HexExpr)
logicalOp = andOp <|> orOp <|> xorOp <|> nandOp

-- Parser para operaciones binarias (separadas en aritméticas y lógicas)
factor :: Parser HexExpr
factor = try binToHexExpr   
     <|> try hexToBinExpr
     <|> try hexToDecExpr
     <|> try decToHexExpr
     <|> try hexNumber
     <|> parens

-- Parser de expresiones con precedencia de multiplicación y división (más alta que las lógicas)
term :: Parser HexExpr
term = chainl1 factor (mulOp <|> divOp)  -- Primero las multiplicaciones y divisiones

-- Parser de expresiones con precedencia de suma y resta (más baja que las lógicas)
exprWithArith :: Parser HexExpr
exprWithArith = chainl1 term (addOp <|> subOp)

-- Parser principal que maneja operaciones lógicas con menor precedencia
parseHexExpr :: Parser HexExpr
parseHexExpr = chainl1 exprWithArith (logicalOp)  -- Primero operaciones aritméticas, luego lógicas

-- Función para parsear un string de entrada
parseHexString :: String -> Either String HexExpr
parseHexString input =
    case parse parseHexExpr "" input of
        Left err -> Left ("Error de parseo: " ++ show err)
        Right expr -> Right expr


        