module Main where

import HexParser (parseHexString, HexExpr)
import HexEvaluator (evalHex)
import System.IO (hFlush, stdout)

-- Función principal
main :: IO ()
main = do
    putStrLn "Bienvenido al evaluador de expresiones hexadecimales."
    putStrLn "Operaciones soportadas: +, -, *, /, and, or, xor, nand, hextodec, dectohex, bintohex, hextobin."
    putStrLn "Ejemplos de entrada:"
    putStrLn "  - 0xA + 0xB"
    putStrLn "  - hextodec 0xFF"
    putStrLn "  - bintohex 1010"
    putStrLn "  - hextobin 0xA"
    putStrLn "Escribe 'salir' para terminar."
    loop

-- Bucle para leer y evaluar expresiones
loop :: IO ()
loop = do
    putStr "> "
    hFlush stdout  -- Asegura que el prompt se muestre inmediatamente
    input <- getLine
    if input == "salir"
        then putStrLn "¡Adiós!"
        else do
            case parseHexString input of
                Left err -> putStrLn $ "Error: " ++ err
                Right expr -> do
                    let result = evalHex expr
                    putStrLn $ "Resultado: " ++ result
            loop  -- Continuar el bucle