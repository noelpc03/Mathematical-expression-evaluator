module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Evaluator (safeEval)
import Parser (parseExpr)
import Text.Parsec (parse)
import Data.List (isInfixOf)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

-- Crear un botón con estilo
createButton :: String -> UI Element
createButton label = UI.button
    # set text label
    # set style [("width", "50px"), ("height", "50px"), ("margin", "3px"),
                 ("font-size", "16px"), ("border-radius", "8px"),
                 ("background-color", "#007BFF"), ("color", "white"),
                 ("border", "none"), ("box-shadow", "1px 1px 3px rgba(0,0,0,0.2)"),
                 ("cursor", "pointer")]

-- Actualizar el campo de entrada con un texto
updateInput :: Element -> String -> UI ()
updateInput input newText = do
    currentText <- get value input
    void $ element input # set value (currentText ++ newText)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Calculator"
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

    void $ return window # set title "Calculadora Científica"

    -- Contenedor principal
    container <- UI.div # set style [("height", "100vh"), ("display", "flex"),
                                     ("justify-content", "center"), ("align-items", "center"),
                                     ("background-color", "#f8f9fa"),
                                     ("padding", "10px")]

    -- Contenedor de la calculadora (responsivo)
    calcBox <- UI.div # set style [("width", "100%"), ("max-width", "450px"),
                                   ("padding", "15px"), ("border", "2px solid #ccc"),
                                   ("border-radius", "10px"),
                                   ("box-shadow", "0 0 10px rgba(0,0,0,0.1)"),
                                   ("background-color", "#fff"),
                                   ("display", "flex"), ("flex-direction", "column"),
                                   ("align-items", "center")]

    -- Campo de entrada
    input <- UI.input # set (attr "placeholder") "Expresión"
                      # set style [("width", "100%"), ("padding", "10px"),
                                   ("font-size", "18px"), ("margin-bottom", "10px"),
                                   ("border-radius", "5px"), ("border", "1px solid #ccc"),
                                   ("text-align", "right")]

    -- Elemento para mostrar el resultado
    output <- UI.div # set style [("margin-top", "10px"), ("font-weight", "bold"),
                                  ("font-size", "18px"), ("text-align", "center")]

    -- Contenedor de botones (responsivo)
    buttonGrid <- UI.div # set style [("display", "flex"), ("flex-wrap", "wrap"),
                                      ("justify-content", "center"), ("gap", "5px")]

    -- Botones de operaciones
    buttons <- mapM createButton ["7", "8", "9", "/", "sin", "asin",
                                  "4", "5", "6", "*", "cos", "acos",
                                  "1", "2", "3", "-", "tan", "atan",
                                  "0", ".", "(", ")", "cot", "acot",
                                  "abs", "^", "sqrt", "+", "∫", "∂",
                                  "=", "C", "CE"]

    -- Añadir botones a la grilla
    void $ element buttonGrid #+ map element buttons

    -- Añadir elementos al contenedor principal
    void $ element calcBox #+ [element input, element buttonGrid, element output]
    void $ element container #+ [element calcBox]
    getBody window #+ [element container]

    -- Mapeo de acciones de botones
    let buttonActions = zip buttons ["7", "8", "9", "/",
                                     "sin(", "asin(", "4", "5", "6", "*",
                                     "cos(", "acos(", "1", "2", "3", "-",
                                     "tan(", "atan(", "0", ".", "(", ")",
                                     "cot(", "acot(", "abs(", "^", "sqrt(", "+",
                                     "integrate(,x)", "deriv(,x)", "=", "C", "CE"]

    mapM_ (\(btn, action) -> on UI.click btn $ \_ -> 
        if action == "=" then do
            expr <- get value input
            result <- liftIO $ case parse parseExpr "" expr of
                                 Left err -> return $ Left ("Error de sintaxis: " ++ show err)
                                 Right e  -> safeEval e
            let resultado = case result of
                              Left errMsg -> "Error: " ++ errMsg
                              Right val   -> "Resultado: " ++ show val
            void $ element output # set text resultado
        else
            handleButtonClick input action) buttonActions

    -- Añadir estilos de Bootstrap
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

handleButtonClick :: Element -> String -> UI ()
handleButtonClick input action =
    if action == "C" then
        void $ element input # set value ""
    else if action == "CE" then
        do currentText <- get value input
           void $ element input # set value (if null currentText then "" else init currentText)
    else
        updateInput input action

