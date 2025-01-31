module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Evaluator (eval)
import Parser (parseExpr)
import Text.Parsec (parse)
import Data.List (isInfixOf)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Evaluador de Expresiones Matemáticas"
    
    -- Crear elementos de la interfaz
    title <- UI.h1 # set text "Evaluador de Expresiones Matemáticas"
    input <- UI.input # set (attr "placeholder") "Introduce una expresión"
    button <- UI.button # set text "Evaluar"
    output <- UI.span
    errorMsg <- UI.div # set style [("color", "red")]
    
    -- Crear el layout
    layout <- UI.div #. "container" #+ 
        [ element title
        , UI.div #. "input-group" #+ [element input, element button]
        , UI.div #. "output-group" #+ [element output, element errorMsg]
        ]
    
    getBody window #+ [element layout]
    
    -- Manejar eventos
    on UI.click button $ \_ -> do
        expr <- get value input
        let resultado = case parse parseExpr "" expr of
                          Left _ -> "Error de sintaxis"
                          Right e -> "Resultado: " ++ show (eval e)
        element output # set text resultado
        element errorMsg # set text (if "Error" `isInfixOf` resultado then resultado else "")
    
    -- Agregar estilos
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
    return ()