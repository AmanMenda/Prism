module ErrorManager (module ErrorManager) where
import GHC.OldList (find)
import GHC.ExecutionStack (Location(functionName))
import Data.Char (isDigit)

isNumbers :: String -> Bool
isNumbers [] = True
isNumbers [a] = isDigit a
isNumbers ('-':j) = isNumbers j
isNumbers (i:j) = isDigit i && isNumbers j

variableExist :: [String] -> String -> Bool
variableExist tab variable = case tab of
    [] -> False
    (i:j) -> (i == variable) || variableExist j variable

checkWellVariableUse :: [String] -> [String] -> String -> Maybe String
checkWellVariableUse [] tab functionName = Nothing
checkWellVariableUse function tab functionName = case function of
    "let": var : ints -> if variableExist tab var then
        Just ("Code Error: In function " ++ functionName ++ " variable >> " ++ var ++ " << already exist")
        else checkWellVariableUse ints (var:tab) functionName

    var : "=" : ints -> if variableExist tab var then checkWellVariableUse ints tab functionName
        else Just ("Code Error: In function " ++ functionName ++ " variable >> " ++ var ++ " << not declared")
    
    func : "(" : ints -> checkWellVariableUse ints tab functionName

    tokens : ints -> do
        let operators_del = ["(", ")", "{", "}", "=", "+=", "++", "--", "-=", "*=", "/=", "+", "-", "*", "/", "%", ">", ">=", "==", "<=", "<", ";", ","]
        let keywords = ["if", "else", "while", "ret"]

        if (isNumbers tokens) then checkWellVariableUse ints tab functionName
        else
            if tokens `elem` operators_del || tokens `elem` keywords then
                checkWellVariableUse ints tab functionName
            else
                case variableExist tab tokens of
                    True -> checkWellVariableUse ints tab functionName
                    False -> Just ("Code Error: In function " ++ functionName ++ " variable >> " ++ tokens ++ " << not declared")

removeElement :: Eq (a) => [a] -> [a] -> [a]
removeElement toRemove [] = []
removeElement toRemove (i:j) = if i `elem` toRemove then
                                removeElement toRemove j
                                else i : removeElement toRemove j

getFirstVariable :: [String] -> [String]
getFirstVariable [] = []
getFirstVariable code = removeElement ["(", ")", ","] (takeWhile (/= ")") (dropWhile (/= "(") code))

checkFileVariableUse :: [[String]] -> Maybe String
checkFileVariableUse [] = Nothing
checkFileVariableUse (i:j) = case checkWellVariableUse (dropWhile (/= "{") i) (getFirstVariable i) (i !! 1) of
    Nothing -> checkFileVariableUse j
    Just errors -> Just errors

functionWellDeclared :: [(String, Int)] -> String -> Bool
functionWellDeclared a function = case a of
    [] -> False
    (fn, position) : functions -> (fn == function) || functionWellDeclared functions function

checkFunctionExistance :: [(String, Int)] -> [String] -> Maybe String
checkFunctionExistance functions [] = Nothing
checkFunctionExistance functions ints = case ints of
    "if" : "(" : suits -> checkFunctionExistance functions suits
    "while" : "(" : suits -> checkFunctionExistance functions suits
    "ret" : "(" : suits -> checkFunctionExistance functions suits
    function : "(" : suits ->
        if functionWellDeclared functions function then checkFunctionExistance functions suits
        else Just function
    _ : suits -> checkFunctionExistance functions suits

checkFunctionExistanceInFile :: [(String, Int)] -> [[String]] -> Maybe String
checkFunctionExistanceInFile functions codeFile = case codeFile of
    [] -> Nothing
    (i:j) -> case checkFunctionExistance functions i of
        Nothing -> checkFunctionExistanceInFile functions j
        Just badFunction ->
            Just ("Code Error: In function " ++ (i !! 1) ++ " usage of an undeclared function: " ++ badFunction)

-- No main

checkForMain :: [[String]] -> Maybe String
checkForMain functions = case functions of
    [] -> Just "Code Error: No main function in your program"
    i:j -> case i of
        "fn":"main":insts -> Nothing
        _ -> checkForMain j

-- Parenthesis and accolades check

findParenthesis :: [String] -> Int -> Int
findParenthesis [] i = i
findParenthesis atoms i = case atoms of
    "(":insts -> findParenthesis insts (i + 1)
    ")":insts -> findParenthesis insts (i - 1)
    "{":insts -> findParenthesis insts (i + 1)
    "}":insts -> findParenthesis insts (i - 1)
    _:insts -> findParenthesis insts i

fileParenthesisCheck :: [[String]] -> Maybe String
fileParenthesisCheck a = case a of
    [] -> Nothing
    (i:j) -> do
        let k = findParenthesis i 0
        case k of
            0 -> fileParenthesisCheck j
            _ ->
                Just ("Syntax Error: Curly bracket or parenthesis not properly placed in function " ++ (i !! 1))

-- Check good function creation

functionDeclarationCheck :: [String] -> Maybe String
functionDeclarationCheck a = case a of
    "fn":name:"(":suits -> Nothing
    _ -> Just "Syntax Error: Function not declared properly"

fileFunctionCheck :: [[String]] -> Maybe String
fileFunctionCheck a = case a of
    [] -> Nothing
    (i:j) -> case functionDeclarationCheck i of
        Nothing -> fileFunctionCheck j
        Just errors -> Just errors
