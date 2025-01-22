module Parser (module Parser) where

import Data.List()
import Data.Maybe()
import Data.Char (isDigit)
import Control.Monad (msum)
import System.IO (openFile)
import GHC.IO.Handle (hGetContents)
import GHC.IO.IOMode (IOMode(ReadMode))
import ErrorManager ( fileFunctionCheck, functionDeclarationCheck, fileParenthesisCheck, checkForMain, checkFunctionExistanceInFile, checkFileVariableUse, isNumbers )

data ASType = Normal | Argument
  deriving (Show, Eq)

data AST = Fn String [AST] [AST]
         | Let String [AST]
         | ASTSymbols String
         | ASTInteger Int
         | ASTError String
         | Param (String, Int)
         | Call Int [AST]
         | Builtins String [AST]
         | If AST [AST] [AST]
         | Else [AST]
         | While AST [AST]
         | Assign String [AST]
         | Ret [AST]
         deriving (Show)

getWords :: String -> Char -> String
getWords "" c = ""
getWords " " c = ""
getWords (i:j) c = if i /= c then i : getWords j c
                 else getWords "" c

getLastWords :: String -> Char -> String
getLastWords "" c = ""
getLastWords (i:j) c = if i == c then j
                 else getLastWords j c

stringSplit :: String -> Char -> [String]
stringSplit "" c = []
stringSplit (i:j) c = case getWords (i:j) c of
    "" -> stringSplit(getLastWords (i:j) c) c
    str -> str:stringSplit(getLastWords (i:j) c) c

deleteComments :: [String] -> [String]
deleteComments [] = []
deleteComments (i:j) = case i of
    ('/':'/':_) -> deleteComments j
    _ -> i : deleteComments j

loadCodeFile :: String -> IO (Maybe String)
loadCodeFile a = case a of
    "" -> return Nothing
    _  -> do
        file <- openFile a ReadMode
        fileContents <- hGetContents file
        return (Just fileContents)

mergeCode :: String -> [String] -> [String]
mergeCode a [] = [a]
mergeCode a (i:j) = case i of
    ('f':'n':_ :xs) -> a : mergeCode i j
    _ -> mergeCode (a ++ i) j

getNegNumber :: String -> String
getNegNumber [] = []
getNegNumber (i:j) = if isDigit i then i : getNegNumber j
                    else []

getAfterNumber :: String -> String
getAfterNumber [] = []
getAfterNumber (i:j) = if isDigit i then getAfterNumber j
                    else (i:j)

codeStringLib :: String -> String
codeStringLib str = case str of
    "" -> []
    (i:j) -> case i of
        '(' -> " ( " ++ codeStringLib j
        ')' -> " ) " ++ codeStringLib j
        ',' -> " , " ++ codeStringLib j
        ';' -> " ; " ++ codeStringLib j

        '+' -> case j of
            '=':k -> " += " ++ codeStringLib k
            '+':k -> " ++ 1 " ++ codeStringLib k
            _ -> " + " ++ codeStringLib j

        '-' -> case j of
            '=':k -> " -= " ++ codeStringLib k
            '-':k -> " -- 1 " ++ codeStringLib k
            val -> case getNegNumber val of
                [] -> " - " ++ codeStringLib j
                nb -> " -" ++ nb ++ " " ++ codeStringLib (getAfterNumber val)
            _ -> " - " ++ codeStringLib j

        '*' -> case j of
            '=':k -> " *= " ++ codeStringLib k
            _ -> " * " ++ codeStringLib j

        '/' -> case j of
            '=':k -> " /= " ++ codeStringLib k
            _ -> " / " ++ codeStringLib j

        '%' -> " % " ++ codeStringLib j

        '>' -> case j of
            '=':k -> " >= " ++ codeStringLib k
            _ -> " > " ++ codeStringLib j

        '<' -> case j of
            '=':k -> " <= " ++ codeStringLib k
            _ -> " < " ++ codeStringLib j

        '=' -> case j of
            '=':k -> " == " ++ codeStringLib k
            _ -> " = " ++ codeStringLib j
        
        '!' -> case j of
            '=':k -> " != " ++ codeStringLib k
            _ -> codeStringLib j

        _ -> i:codeStringLib j

createTabFromString :: String -> [String]
createTabFromString a = stringSplit (codeStringLib a) ' '

codeFunctionTab :: [String] -> [[String]]
codeFunctionTab func = case func of
    [] -> []
    (i:j) -> case createTabFromString i of
        [] -> codeFunctionTab j
        _ -> createTabFromString i : codeFunctionTab j

skipNestedBlocks :: [String] -> [String]
skipNestedBlocks tokens = skips tokens 0 where
    skips [] _ = []
    skips ("{":ts) n = skips ts (n+1)
    skips ("}":ts) 1 = ts
    skips ("}":ts) n = skips ts (n-1)
    skips (_:ts) n = skips ts n

skipNestedBlocks2 :: [String] -> [String]
skipNestedBlocks2 tokens = skips tokens 0 [] where
    skips [] _ acc = reverse acc
    skips ("{":ts) 0 acc = skips ts 1 ("{":acc)
    skips ("{":ts) n acc = skips ts (n+1) ("{":acc)
    skips ("}":ts) 1 acc = reverse ("}":acc)
    skips ("}":ts) n acc = skips ts (n-1) ("}":acc)
    skips (a:ts) n acc = if n > 0 then skips ts n (a:acc) else skips ts n acc

skipNestedParenthesisBlocks :: [String] -> [String]
skipNestedParenthesisBlocks tokens = skips tokens 0 where
    skips [] _ = []
    skips ("(":ts) n = skips ts (n+1)
    skips (")":ts) 1 = ts
    skips (")":ts) n = skips ts (n-1)
    skips (_:ts) n = skips ts n

--assignOrReturn :: [String] -> [AST]

codeParameters :: [String] -> Int -> [AST] -> [(String, Int)] -> [AST]
codeParameters [] val ast functionIndex = []
codeParameters param val ast functionIndex = case param of
    func:"(":j -> [codeLet (func:"(":j) ast functionIndex]
    [")"] -> []
    "(":j -> codeParameters j val ast functionIndex
    ",":j -> codeParameters j val ast functionIndex
    a:j -> codeParameters j (val + 1) ast functionIndex ++ [Param (a, val)]

codeParametersFunction :: [String] -> [AST] -> [(String, Int)] -> [AST]
codeParametersFunction [] ast funtionIndex = []
codeParametersFunction param ast functionIndex  = case param of
    func:"(":j -> [codeLet (func:"(":j) ast functionIndex]
    [")"] -> []
    "(":j -> codeParametersFunction j ast functionIndex
    ",":j -> codeParametersFunction j ast functionIndex

    a:j -> if isNumbers a then
        codeParametersFunction j ast functionIndex ++ [ASTInteger (getintValue a)]
        else
            codeParametersFunction j ast functionIndex ++ [getFunctionParameters a ast]

isComparision :: [String] -> Maybe String
isComparision [] = Nothing
isComparision (x:xs) = case x of
    "==" -> Just "=="
    ">=" -> Just ">="
    ">" -> Just ">"
    "<=" -> Just "<="
    "<" -> Just "<"
    _ -> isComparision xs

compareTwoPart :: Maybe String -> [String] -> Maybe ([String], [String])
compareTwoPart Nothing a = Nothing
compareTwoPart (Just str) a = do
    let partone = takeWhile (/= str) a
    let parttwo = dropWhile (/= str) a
    Just (partone, parttwo)

getComparisionAst :: [String] -> [AST] -> [(String, Int)] -> [AST]
getComparisionAst a ast functionIndex = do
    let comparision = isComparision a
    case comparision of
        Nothing -> []
        Just comparator ->
            case compareTwoPart comparision a of
                Nothing -> []
                Just (partone, parttwo) ->
                    [Builtins comparator [codeLet parttwo ast functionIndex, codeLet partone ast functionIndex]]

getFunctionParameters :: String -> [AST] -> AST
getFunctionParameters a [] = ASTSymbols a
getFunctionParameters a (i:j) = case i of
    Param (str, value) -> if a == str then i
                            else getFunctionParameters a j

parserToAst :: [String] -> [AST] -> [(String, Int)] -> [AST]
parserToAst code fnParameters functionIndex = case code of

    ";":codeSuite -> parserToAst codeSuite fnParameters functionIndex

    "}":codeSuite -> parserToAst codeSuite fnParameters functionIndex

    "{":codeSuite -> parserToAst codeSuite fnParameters functionIndex

    ")":codeSuite -> parserToAst codeSuite fnParameters functionIndex

    "(":codeSuite -> parserToAst codeSuite fnParameters functionIndex

    "fn":functionName:parameters -> case dropWhile (/= "{") parameters of
        "{":functionContent -> do
            let fnParam = codeParameters (takeWhile (/= "{") parameters) 0
            [Fn functionName (fnParam [] functionIndex) (parserToAst functionContent (fnParam [] functionIndex) functionIndex)]
        _ -> error "fn error"

    "if":conditionIns -> case dropWhile (/= "{") conditionIns of
        "{":ifContent -> do
            let conditionParameters = takeWhile (/= ")") conditionIns 
            let c = filter (/= "(") conditionParameters
            let d = filter (/= ")") c
            let compare = head (getComparisionAst (takeWhile (/= ")") d) fnParameters functionIndex)
            case skipNestedBlocks conditionIns of
                "else":conditionElse -> case skipNestedBlocks conditionElse of
                    "}":follow -> [If compare (parserToAst ifContent fnParameters functionIndex) (parserToAst (skipNestedBlocks2 conditionElse) fnParameters functionIndex)]
                    _:follow -> If compare (parserToAst ifContent fnParameters functionIndex) (parserToAst (skipNestedBlocks2 conditionElse) fnParameters functionIndex) : parserToAst (skipNestedBlocks conditionElse) fnParameters functionIndex
                "}":follow -> [If compare (parserToAst ifContent fnParameters functionIndex) []]
                _ -> If compare (parserToAst ifContent fnParameters functionIndex) [] : parserToAst (skipNestedBlocks conditionIns) fnParameters functionIndex
        _ -> error "if error"
    "else":conditionIns -> case conditionIns of
        "{":elseContent -> 
            case (skipNestedBlocks ("{":elseContent)) of
                "}":follow -> (parserToAst elseContent fnParameters functionIndex)
                _:follow -> (parserToAst elseContent fnParameters functionIndex) ++ parserToAst (skipNestedBlocks ("{":elseContent)) fnParameters functionIndex

    "while":conditionIns -> case dropWhile (/= "{") conditionIns of
        "{":whileContent -> do
            let conditionParameters = takeWhile (/= ")") conditionIns
            let c = filter (/= "(") conditionParameters
            let d = filter (/= ")") c
            let compare = head (getComparisionAst (takeWhile (/= ")") d ) fnParameters functionIndex)
            While compare (parserToAst whileContent fnParameters functionIndex) : parserToAst (skipNestedBlocks conditionIns) fnParameters functionIndex
        _ -> error "while error"

    "let":variableName:"=":value ->
        Let variableName [codeLet (takeWhile (/= ";") value) fnParameters functionIndex] : parserToAst (dropWhile (/= ";") value) fnParameters functionIndex

    "ret":"(":retValue -> [Ret [codeLet (takeWhile (/= ")") retValue) fnParameters functionIndex]]

    function:"(":suits -> case findFonctionIndex function functionIndex of
        Just i ->
            Call i (codeParametersFunction (takeWhile (/= ")") suits) fnParameters functionIndex) : parserToAst (dropWhile (/= ")") suits) fnParameters functionIndex
        Nothing -> error "Empty File"

    var : "=" : suits -> case dropWhile (/= ";") suits of
        ";" : "}" : suit ->
            [Assign var [codeLet (takeWhile (/= ";") suits) fnParameters functionIndex]]
        ";" : _ : suit -> 
            Assign var [codeLet (takeWhile (/= ";") suits) fnParameters functionIndex] : parserToAst (dropWhile (/= ";") suits) fnParameters functionIndex
        _ -> error "Bad Assignment"

    var : opEqual : value : suits -> case dropWhile (/= ";") suits of
        ";":"}":suit -> case opEqual of
            "+=" -> [Assign var [Builtins "+" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]]]
            "-=" -> [Assign var [Builtins "-" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]]]
            "*=" -> [Assign var [Builtins "*" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]]]
            "/=" -> [Assign var [Builtins "/" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]]]
            "++" -> [Assign var [Builtins "+" [getFunctionParameters var fnParameters, ASTInteger 1]]]
            "--" -> [Assign var [Builtins "-" [ASTInteger 1, getFunctionParameters var fnParameters]]]
        ";":suit -> case opEqual of
            "+=" -> Assign var [Builtins "+" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]] : parserToAst suit fnParameters functionIndex
            "-=" -> Assign var [Builtins "-" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]] : parserToAst suit fnParameters functionIndex
            "*=" -> Assign var [Builtins "*" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]] : parserToAst suit fnParameters functionIndex
            "/=" -> Assign var [Builtins "/" [getFunctionParameters var fnParameters, ASTInteger (getintValue value)]] : parserToAst suit fnParameters functionIndex
            "++" -> Assign var [Builtins "+" [getFunctionParameters var fnParameters, ASTInteger 1]] : parserToAst suit fnParameters functionIndex
            "--" -> Assign var [Builtins "-" [ASTInteger 1, getFunctionParameters var fnParameters]] : parserToAst suit fnParameters functionIndex
    _ -> []

getintValue :: String -> Int
getintValue ('-':number) = (read number) * (-1)
getintValue number = read number

codeLet :: [String] -> [AST] -> [(String, Int)] -> AST
codeLet a ast functionIndex = case a of
    [value] -> if isNumbers value then
                    ASTInteger (getintValue value)
                else
                    getFunctionParameters value ast
    function:"(":suits -> case findFonctionIndex function functionIndex of
        Just i -> Call i (codeParametersFunction (takeWhile (/= ")") suits) ast functionIndex)
        Nothing -> error "Empty File"

    var:builtins:var2:rest -> case builtins of
        "+" -> Builtins builtins [codeLet [var2] ast functionIndex, codeLet [var] ast functionIndex]
        "-" -> Builtins builtins [codeLet [var2] ast functionIndex, codeLet [var] ast functionIndex]
        "*" -> Builtins builtins [codeLet [var2] ast functionIndex, codeLet [var] ast functionIndex]
        "/" -> Builtins builtins [codeLet [var2] ast functionIndex, codeLet [var] ast functionIndex]
        "%" -> Builtins builtins [codeLet [var2] ast functionIndex, codeLet [var] ast functionIndex]
        _ -> error ("What do you mean with opertor between " ++ var ++ " and " ++ var2)
    _:suits -> codeLet suits ast functionIndex
    _ -> error "Unexpected pattern"

functionListIndex :: [[String]] -> Int -> [(String, Int)]
functionListIndex [] functionValue = []
functionListIndex (i:j) functionValue = case i of
    "fn":functionName:function ->
        (functionName, functionValue) : functionListIndex j (functionValue + 1)
    _ -> []

findFonctionIndex :: String -> [(String, Int)] -> Maybe Int
findFonctionIndex a [] = Nothing
findFonctionIndex a (i:j) = case i of
    (function, value) -> if function == a then Just value
                        else findFonctionIndex a j

codeFileParsing :: [[String]] -> [[String]] -> [AST]
codeFileParsing [] a = []
codeFileParsing (i:j) a = head (parserToAst i [] (functionListIndex a 0)) : codeFileParsing j a

errorManager :: [[String]] -> [(String, Int)] -> Maybe String
errorManager code functions = msum errors
    where
        functionError = fileFunctionCheck code
        parenthesisError = fileParenthesisCheck code
        noMainError = checkForMain code
        functionExistenceError = checkFunctionExistanceInFile functions code
        variableError = checkFileVariableUse code
        errors = [functionError, parenthesisError, noMainError, functionExistenceError, variableError]

fileRead :: Maybe String -> [AST]
fileRead maybeContents = case maybeContents of
        Nothing -> [ASTError "Empty file"]
        Just contents -> do
            let functionsLines =  mergeCode "" $ deleteComments (stringSplit contents '\n')
            let functions_tab = codeFunctionTab functionsLines
            let functionsIndex = functionListIndex functions_tab 0
            let errorsCheck = errorManager functions_tab functionsIndex

            case errorsCheck of
                Nothing -> codeFileParsing functions_tab functions_tab
                Just errors -> [ASTError errors]