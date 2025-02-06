module HexEvaluator (evalHex) where

import HexParser 
import Numeric (readHex, showHex) 
import Data.Char (intToDigit) 
import Data.Bits (shiftL, shiftR, (.&.)) 

binToDecimal :: String -> Int 
binToDecimal = foldl (\acc bit -> acc * 2 + if bit == '1' then 1 else 0) 0 

decimalToBinary :: Int -> String 
decimalToBinary 0 = "0" 
decimalToBinary n = reverse (helper n) 
    where 
        helper 0 = "" 
        helper x = intToDigit (x `mod` 2) : helper (x `div` 2) 
        
hexToBinary :: String -> String 
hexToBinary hex = decimalToBinary (hexToDecimal hex) 

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
evalHex :: HexExpr -> String 
evalHex (Val hex) = hex 
evalHex (Add x y) = decimalToHex $ (hexToDecimal (evalHex x) + hexToDecimal (evalHex y) )
evalHex (Sub x y) = decimalToHex $ (hexToDecimal (evalHex x) - hexToDecimal (evalHex y) )
evalHex (Mul x y) = decimalToHex $ (hexToDecimal (evalHex x) * hexToDecimal (evalHex y) )
evalHex (Div x y) = decimalToHex $ (hexToDecimal (evalHex x) `div` hexToDecimal (evalHex y) )
evalHex (HexToDec x) = show $ hexToDecimal (evalHex x) 
evalHex (DecToHex (Val x)) = decimalToHex (read x) 
evalHex (BinToHex (Val bin)) = binaryToHex bin 
evalHex (HexToBin (Val hex)) = hexToBinary hex 
evalHex _ = "Operación no válida"


