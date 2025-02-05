module HexEvaluator (evalHex, safeEvalHex) where

import HexParser (HexExpr(..))
import Numeric (showHex, readHex, showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import qualified Control.Exception as Exception
import Control.Exception (SomeException, evaluate)
import Data.Bits ((.&.), (.|.), xor)  -- Añade esta línea
import Data.List (unfoldr)

-- Función para convertir de decimal a hexadecimal
decimalToHex :: Int -> String
decimalToHex n = showHex n ""

-- Función para convertir de hexadecimal a decimal
hexToDecimal :: String -> Int
hexToDecimal hex = fst . head . readHex $ hex  -- Convierte el número hexadecimal en decimal

-- Evaluador para operaciones con valores hexadecimales
evalHex :: HexExpr -> String
evalHex (Val n)     = show n
evalHex (Add x y)   = decimalToHex $ hexToDecimal (evalHex x) + hexToDecimal (evalHex y)
evalHex (Sub x y)   = decimalToHex $ hexToDecimal (evalHex x) - hexToDecimal (evalHex y)
evalHex (Mul x y)   = decimalToHex $ hexToDecimal (evalHex x) * hexToDecimal (evalHex y)
evalHex (Div x y)   = decimalToHex $ hexToDecimal (evalHex x) `div` hexToDecimal (evalHex y)
evalHex (HexToDec x) = show $ hexToDecimal (evalHex x)
evalHex (DecToHex (Val n)) = decimalToHex (round n)
evalHex (HexToBin x) = show $ hexToBinary (evalHex x)
evalHex (BinToHex x) = binaryToHex (evalHex x)
evalHex (HexAnd x y) = decimalToHex $ hexToDecimal (evalHex x) .&. hexToDecimal (evalHex y)
evalHex (HexOr x y)  = decimalToHex $ hexToDecimal (evalHex x) .|. hexToDecimal (evalHex y)
evalHex (HexXor x y) = decimalToHex $ hexToDecimal (evalHex x) `xor` hexToDecimal (evalHex y)
evalHex _ = "Operación hexadecimal no válida"

-- Función para convertir binario a hexadecimal directamente
binaryToHex :: String -> String
binaryToHex ('0':'b':bin) = "0x" ++ convertBinToHex bin
binaryToHex _ = error "Formato binario inválido"

-- Convierte una cadena binaria en su representación hexadecimal
convertBinToHex :: String -> String
convertBinToHex bin = map binGroupToHex (padLeft (length bin) bin)

-- Agrupa de 4 en 4 y convierte a hexadecimal
binGroupToHex :: String -> Char
binGroupToHex group = intToDigit (foldl (\acc x -> acc * 2 + (if x == '1' then 1 else 0)) 0 group)

-- Asegura que la cantidad de bits sea múltiplo de 4
padLeft :: Int -> String -> [String]
padLeft len bin =
    let padding = (4 - (len `mod` 4)) `mod` 4
        paddedBin = replicate padding '0' ++ bin
    in unfoldr (\b -> if null b then Nothing else Just (take 4 b, drop 4 b)) paddedBin

-- Función para convertir hexadecimal a binario directamente
hexToBinary :: String -> String
hexToBinary ('0':'x':hex) = "0b" ++ concatMap hexDigitToBin hex
hexToBinary _ = error "Formato hexadecimal inválido"

-- Convierte un dígito hexadecimal a su representación binaria de 4 bits
hexDigitToBin :: Char -> String
hexDigitToBin c = let n = digitToInt c in padLeftBin (showIntAtBase 2 intToDigit n "")

-- Asegura que la cantidad de bits sea múltiplo de 4
padLeftBin :: String -> String
padLeftBin bin = replicate (4 - length bin) '0' ++ bin

-- Evaluación segura que captura excepciones
safeEvalHex :: HexExpr -> IO (Either String String)
safeEvalHex expr = do
    result <- Exception.try (evaluate (evalHex expr)) :: IO (Either SomeException String)
    return $ case result of
        Left ex  -> Left ("Error al evaluar la expresión hexadecimal: " ++ show ex)
        Right val -> Right val
