module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Evaluator (safeEval,exprToString)
import Parser (parseExpr)
import Text.Parsec (parse)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

-- Crear un botón con estilo
createButton :: String -> UI Element
createButton label = UI.button
    # set text label
    # set style [("width", "60px"), ("height", "60px"), ("margin", "3px"),
                 ("font-size", "18px"), ("border-radius", "8px"),
                 ("background-color", "#007BFF"), ("color", "white"),
                 ("border", "none"), ("box-shadow", "1px 1px 3px rgba(0,0,0,0.2)"),
                 ("cursor", "pointer")]

createSwitchButton :: String -> UI Element
createSwitchButton label = UI.button
    # set text label
    # set style [("width", "110px"), ("height", "40px"), ("margin", "3px"),
                 ("font-size", "14px"), ("border-radius", "6px"),
                 ("background-color", "#28A745"), ("color", "white"),
                 ("border", "none"), ("cursor", "pointer")]

-- Bloquear la entrada del teclado excepto "x" en decimal
restrictInput :: Element -> Maybe String -> UI ()
restrictInput input (Just allowedChars) = do
    on UI.keyup input $ \_ -> do
        text <- get value input
        let filteredText = filter (`elem` allowedChars) text
        when (text /= filteredText) $ void $ element input # set value filteredText
restrictInput _ Nothing = return ()

updateInput :: Element -> String -> UI ()
updateInput input newText = do
    currentText <- get value input
    void $ element input # set value (currentText ++ newText)

handleButtonClick :: Element -> String -> UI ()
handleButtonClick input action =
    if action == "C" then
        void $ element input # set value ""
    else if action == "CE" then do
        currentText <- get value input
        void $ element input # set value (if null currentText then "" else init currentText)
    else
        updateInput input action

updateButtonGrid :: [(String, String)] -> Element -> Element -> UI ()
updateButtonGrid buttonActions grid input = do
    buttons <- mapM (createButton . fst) buttonActions
    mapM_ (\(btn, action) -> on UI.click btn $ \_ -> 
        if action == "=" then do
            expr <- get value input
            result <- liftIO $ case parse parseExpr "" expr of
                                 Left _  -> return (Left "Error")
                                 Right e -> do
                                        safeResult <- safeEval e
                                        return $ case safeResult of
                                                Left errMsg -> Left errMsg
                                                Right expr  -> Right (exprToString expr)                      
            let resultado = either id show result
            currentText <- get value input
            void $ element input # set value (currentText ++ "\n" ++ resultado)
        else
            handleButtonClick input action) (zip buttons (map snd buttonActions))

    void $ element grid # set children buttons

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Calculadora"
    UI.addStyleSheet window "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"

    body <- getBody window
    void $ element body # set style [("height", "100vh"), ("display", "flex"),
                                     ("justify-content", "center"), ("align-items", "center"),
                                     ("background", "linear-gradient(135deg, #1E3C72, #2A5298)"),
                                     ("padding", "10px"), ("margin", "0")]

    calcBox <- UI.div # set style [("width", "80%"), ("max-width", "500px"),
                                   ("padding", "20px"), ("border", "2px solid #ccc"),
                                   ("border-radius", "10px"),
                                   ("background-color", "#fff"),
                                   ("box-shadow", "0 4px 15px rgba(0,0,0,0.2)"),
                                   ("display", "flex"), ("flex-direction", "column"),
                                   ("align-items", "center")]

    input <- UI.textarea # set (attr "placeholder") "Expresión"
                         # set style [("width", "100%"), ("height", "60px"),
                                      ("padding", "10px"), ("font-size", "20px"),
                                      ("text-align", "right"), ("border-radius", "5px"),
                                      ("border", "1px solid #ccc"), ("resize", "none"),
                                      ("overflow", "hidden")]

    buttonGrid <- UI.div # set style [("display", "grid"), ("grid-template-columns", "repeat(6, 1fr)"),
                                      ("gap", "5px"), ("justify-content", "center")]

    buttonsSwitch <- mapM createSwitchButton ["Decimal", "Binario", "Hexadecimal"]
    switchContainer <- UI.div # set style [("display", "flex"), ("justify-content", "center"),
                                           ("margin-bottom", "10px"), ("gap", "5px")]
    void $ element switchContainer #+ map element buttonsSwitch

    void $ element calcBox #+ [element switchContainer, element input, element buttonGrid]
    void $ element body #+ [element calcBox]

    let switchSystem buttons allowedChars = do
            updateButtonGrid buttons buttonGrid input
            restrictInput input allowedChars
            void $ element input # set value ""

    on UI.click (head buttonsSwitch) $ \_ -> switchSystem buttonsDecimal (Just "0123456789.,+-*/()^ex")
    on UI.click (buttonsSwitch !! 1) $ \_ -> switchSystem buttonsBin (Just "01+-*/()")
    on UI.click (last buttonsSwitch) $ \_ -> switchSystem buttonsHex (Just "0123456789ABCDEF+-*/()")

    switchSystem buttonsDecimal (Just "0123456789.,+-*/()^ex")

buttonsDecimal :: [(String, String)]
buttonsDecimal =
    [("C", "C"), ("CE", "CE"), ("(", "("), (")", ")"), ("^", "^"), ("/", "/")
    ,("7", "7"), ("8", "8"), ("9", "9"), ("*", "*"), ("sqrt", "sqrt("), ("abs", "abs(")
    ,("4", "4"), ("5", "5"), ("6", "6"), ("-", "-"), ("sin", "sin("), ("cos", "cos(")
    ,("1", "1"), ("2", "2"), ("3", "3"), ("+", "+"), ("tan", "tan("), ("cot", "cot(")
    ,("0", "0"), (",", ","), (".", "."), ("=", "="), ("∫", "integrate ("), ("∂", "deriv(")
    ,("ln", "ln("), ("log", "log("), ("e", "e"), ("𝝅","PI"), ("x", "x")
    ,("asin", "asin("), ("acos", "acos("), ("atan", "atan("), ("sec", "sec("), ("csc", "csc("),("bin","dectobin "),("hex","dectohex ")]

buttonsBin :: [(String, String)]
buttonsBin =
    [("C", "C"), ("CE", "CE"), ("(", "("), (")", ")"), ("1", "1"), ("0", "0")
    ,("AND", "and ("), ("OR", "or ("), ("XOR", "xor ("), ("NAND", "nand ("), ("+", "+b ("), ("-", "-b (")
    , ("dec", "bintodec "), ("hex","bintohex "),("=", "="),(",",",")]

buttonsHex :: [(String, String)]
buttonsHex =
    [("C", "C"), ("CE", "CE"), ("(", "("), (")", ")"), ("+", "+h ("), ("-", "-h (")
    ,("A", "a"), ("B", "b"), ("C", "c"), ("D", "d"), ("E", "e"), ("F", "f")
    ,("7", "7"), ("8", "8"), ("9", "9"), ("0", "0")
    ,("4", "4"), ("5", "5"), ("6", "6"), ("bin", "hextobin "), ("dec", "hextodec "), ("=", "=")
    ,("1", "1"), ("2", "2"), ("3", "3"),("x","x"),(",",",")]

