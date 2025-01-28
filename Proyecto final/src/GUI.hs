module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Evaluator (eval)
import Parser (parseExpr)
import Text.Parsec (parse)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "My Application"
    button <- UI.button # set text "Click me"
    input <- UI.input
    output <- UI.span
    layout <- UI.div #+ [element button, element input, element output]
    getBody window #+ [element layout]

    on UI.click button $ \_ -> do
        expr <- get UI.value input
        let resultado = case parse parseExpr "" expr of
                          Left _ -> "Error de sintaxis"
                          Right e -> "Resultado: " ++ show (eval e)
        element output # set UI.text resultado