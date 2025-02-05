module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Evaluator (safeEval)
import Parser (parseExpr)
import Text.Parsec (parse)
import Data.List (isInfixOf)
import Control.Monad (void)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    void $ return window # set title "Evaluador de Expresiones Matemáticas"
    
    -- Contenedor principal para centrar todo en la ventana (usando Flexbox)
    container <- UI.div # set style [("height", "100vh"),
                                     ("display", "flex"),
                                     ("justify-content", "center"),
                                     ("align-items", "center"),
                                     ("background-color", "#f8f9fa")]
    
    -- Recuadro central (estilo card)
    box <- UI.div # set style [ ("width", "500px")
                              , ("padding", "40px 20px 60px 20px")
                              , ("border", "1px solid #ccc")
                              , ("border-radius", "5px")
                              , ("box-shadow", "0 0 10px rgba(0,0,0,0.1)")
                              , ("background-color", "#fff")
                              , ("position", "relative")
                              , ("text-align", "center")
                              ]
    
    -- Título grande
    header <- UI.h1 # set text "Evaluador Matemático"
    
    -- Oración pequeña debajo del título
    subheader <- UI.p # set text "Introduce tu expresión y evalúa sus resultados"
    
    -- Campo de entrada para la expresión
    input <- UI.input # set (attr "placeholder") "Expresión"
                        # set style [("width", "80%"), ("margin", "20px 0"), ("padding", "10px")]
    
    -- Elemento para mostrar el resultado y mensajes de error
    output <- UI.div # set style [("margin-top", "10px"), ("font-weight", "bold")]
    errorMsg <- UI.div # set style [("color", "red"), ("margin-top", "10px")]
    
    -- Botón para evaluar, colocado en la esquina inferior derecha del recuadro
    button <- UI.button # set text "Evaluar"
                        # set style [("position", "absolute"), ("bottom", "20px"), ("right", "20px")]
    
    -- Agregar los elementos al recuadro
    void $ element box #+ [ element header
                          , element subheader
                          , element input
                          , element output
                          , element errorMsg
                          , element button
                          ]
    
    -- Agregar el recuadro al contenedor principal
    void $ element container #+ [ element box ]
    
    -- Agregar el contenedor al body de la ventana
    getBody window #+ [ element container ]
    
    -- Manejar el evento de click en el botón para evaluar la expresión
    on UI.click button $ \_ -> do
        expr <- get value input
        result <- liftIO $ case parse parseExpr "" expr of
                             Left err -> return $ Left ("Error de sintaxis: " ++ show err)
                             Right e  -> safeEval e
                             
        let resultado = case result of
                          Left errMsg -> errMsg
                          Right (Left str) -> "Resultado: " ++ str  -- Si es un String (binario)
                          Right (Right num) -> "Resultado: " ++ show num  -- Si es un número (Double)

        element output # set text resultado
        element errorMsg # set text (if "Error" `isInfixOf` resultado then resultado else "")

    -- Agregar la hoja de estilos de Bootstrap
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
