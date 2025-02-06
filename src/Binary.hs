module Binary (binaryToDecimal, decimalToBinary, evalBinOp) where

import Debug.Trace (trace)
import Data.Bits (xor, (.&.), (.|.), complement)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Función auxiliar para convertir un binario (string) a decimal
binaryToDecimal :: String -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + read [x]) 0

-- Función auxiliar para convertir un número decimal a binario
decimalToBinary :: Int -> String
decimalToBinary 0 = "0"
decimalToBinary n = reverse (showIntAtBase 2 intToDigit n "")


-- Convierte un número binario en una lista de bits
stringToBits :: String -> [Int]
stringToBits = map (read . (:[]))

-- Convierte una lista de bits a un número binario
bitsToString :: [Int] -> String
bitsToString = map (\x -> if x==1 then '1' else '0')

-- Añade ceros a la izquierda para igualar las longitudes de las cadenas binarias
padWithZeros :: Int -> String -> String
padWithZeros len str = replicate (len - length str) '0' ++ str

-- XOR bit a bit empezando desde el final (derecha a izquierda)
xorBitByBit :: String -> String -> String
xorBitByBit x y = bitsToString $ zipWith xor (stringToBits (reverse x)) (stringToBits (reverse y))

-- AND bit a bit empezando desde el final (derecha a izquierda)
andBitByBit :: String -> String -> String
andBitByBit x y = bitsToString $ zipWith (.&.) (stringToBits (reverse x)) (stringToBits (reverse y))

-- OR bit a bit empezando desde el final (derecha a izquierda)
orBitByBit :: String -> String -> String
orBitByBit x y = reverse (bitsToString $ zipWith (.|.) (stringToBits (reverse x)) (stringToBits (reverse y)))

-- NAND bit a bit empezando desde el final (derecha a izquierda)
nandBitByBit :: String -> String -> String
nandBitByBit x y = bitsToString $ zipWith (\a b -> complement (a .&. b) .&. 1)(stringToBits (reverse x))(stringToBits (reverse y))


-- Evaluar operaciones binarias bit a bit
evalBinOp :: String -> String -> String -> String
evalBinOp "xor" x y = xorBitByBit (padWithZeros (max (length x) (length y)) x) (padWithZeros (max (length x) (length y)) y)
evalBinOp "and" x y = andBitByBit (padWithZeros (max (length x) (length y)) x) (padWithZeros (max (length x) (length y)) y)
evalBinOp "or" x y  = reverse (orBitByBit (padWithZeros (max (length x) (length y)) x) (padWithZeros (max (length x) (length y)) y))
evalBinOp "nand" x y = nandBitByBit (padWithZeros (max (length x) (length y)) x) (padWithZeros (max (length x) (length y)) y)
evalBinOp _ _ _ = "Operación no válida"
