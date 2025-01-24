module Main where

import Parser (parseExpr)
import Evaluator (eval)
import Text.Parsec (parse)

-- Función para procesar la entrada del usuario
processInput :: String -> IO ()
processInput input
    | input == "S" = putStrLn "Saliendo..."
    | otherwise = case parse parseExpr "" (filter(/=' ')input) of
        Left err -> print err
        Right expr -> do
            putStrLn "Árbol de la expresión:"
            print expr  -- Muestra la estructura del árbol de la expresión
            putStrLn "Resultado de la evaluación:"
            print $ eval expr  -- Muestra el resultado de evaluar la expresión

-- Función principal
main :: IO ()
main = do
    let loop = do
            putStrLn "Ingrese una expresión matemática (o 'S' para salir):"
            input <- getLine
            if input == "S"
                then putStrLn "Saliendo..."
                else do
                    processInput input
                    loop
    loop