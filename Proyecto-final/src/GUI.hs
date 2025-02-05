module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import HexParser (parseHexExpr, HexExpr(..))  -- Importamos HexParser y HexExpr
import HexEvaluator (evalHex)  -- Usamos evalHex para evaluar expresiones hexadecimales
import Text.Parsec (parse)
import Data.List (isInfixOf)

-- Definir la GUI
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Calculadora Hexadecimal"
    
    -- Crear elementos de la interfaz
    title <- UI.h1 # set text "Calculadora Hexadecimal"
    input <- UI.input # set (attr "placeholder") "Introduce una expresión hexadecimal"
    button <- UI.button # set text "Evaluar"
    output <- UI.span
    errorMsg <- UI.div # set style [("color", "red")]
    historyList <- UI.ul
    
    -- Crear el layout
    layout <- UI.div #. "container" #+ 
        [ element title
        , UI.div #. "input-group" #+ [element input, element button]
        , UI.div #. "output-group" #+ [element output, element errorMsg]
        , UI.div #. "history-group" #+ [element historyList]
        ]
    
    getBody window #+ [element layout]
    
    -- Manejar eventos
    on UI.click button $ \_ -> do
        expr <- get value input
        let resultado = case parse parseHexExpr "" expr of
                          Left _ -> "Error de sintaxis"
                          Right e -> "Resultado: " ++ evalHex e
        element output # set text resultado
        element errorMsg # set text (if "Error" `isInfixOf` resultado then resultado else "")
    
    -- Agregar estilos
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
    return ()
