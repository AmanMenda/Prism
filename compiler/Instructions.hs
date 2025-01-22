module Main where

import VM.Types
import BytecodeTranslator.BytecodeGenerator
import BytecodeTranslator.BytecodePrinter

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Parser (
    fileRead,
    loadCodeFile,
    AST (
    ASTInteger,
    ASTError,
    ASTSymbols,
    If,
    While,
    Fn,
    Let,
    Builtins,
    Ret,
    Param,
    Assign, Call ) )

whichBuiltin :: String -> Builtins
whichBuiltin "+" = Add
whichBuiltin "-" = Sub
whichBuiltin "*" = Mul
whichBuiltin "/" = Div
whichBuiltin "==" = Eq
whichBuiltin "<" = Less

generateInstructions :: AST -> [[Instructions]] -> Int -> ([Instructions], [[Instructions]], Int)
generateInstructions (Fn _ _ body) conditions count = 
    let foldlFunc (accInstructions, accConditions, accCount) expr = 
            let (newInstructions, newConditions, newCount) = generateInstructions expr conditions accCount
            in (accInstructions ++ newInstructions, accConditions ++ newConditions, newCount)
        (instructions, updatedConditions, updatedCount) = foldl foldlFunc ([], [], count) body
    in (instructions, updatedConditions, updatedCount)

generateInstructions (Let name exprs) conditions count = 
    let (instructions, _, _) = unzip3 $ map (\expr -> generateInstructions expr conditions count) exprs
    in (concat instructions ++ [StoreValue name], conditions, count)

generateInstructions (Builtins name args) conditions count = 
    let (instructions, _, _) = unzip3 $ map (\expr -> generateInstructions expr conditions count) args
    in (concat instructions ++ [PushValue (Op (whichBuiltin name)), SysCall], conditions, count)

generateInstructions (Ret args) conditions count = 
    let (instructions, _, _) = unzip3 $ map (\expr -> generateInstructions expr conditions count) args
    in (concat instructions ++ [RetValue], conditions, count)

generateInstructions (Call name args) conditions count =
    let argInstructions = [PushArg (case argName of
                ASTInteger integer -> (Integer integer)
                ASTSymbols sym -> (Symbol sym)
                Param (paramName, _) -> (Symbol paramName)
                _ -> error "Unhandled case in generateInstructions for Call")
            | argName <- args]
    in (argInstructions ++ [PushFunc name, SysCall], conditions, count)

generateInstructions (ASTSymbols symbol) conditions count = ([LoadName symbol], conditions, count)

generateInstructions (Assign name exprs) conditions count = 
    let (instructions, _, _) = unzip3 $ map (\expr -> generateInstructions expr conditions count) exprs
    in (concat instructions ++ [StoreValue name], conditions, count)

generateInstructions (Param (_, index)) conditions count = ([LoadArg index], conditions, count)

generateInstructions (ASTInteger value) conditions count = ([PushValue (Integer value)], conditions, count)

generateInstructions (If condition iftrue iffalse) labels count =
    let (conditionInstructions, updatedCondlabels, updatedCondCount) = generateInstructions condition labels count
    
        foldlFunc (accInstructions, accLabels, accCount) expr =
            let (instructions, newLabels, newCount) = generateInstructions expr updatedCondlabels accCount
            in (accInstructions ++ instructions, accLabels ++ newLabels, newCount)
    
        (iftrueInstructions, iftrueLabels, iftrueCount) = foldl foldlFunc ([], [], (updatedCondCount + 2)) iftrue
    
        (iffalseInstructions, iffalseLabels, _) = foldl foldlFunc ([], [], iftrueCount) iffalse
    
        finalIftrueLabels = updatedCondlabels ++ [iftrueInstructions] ++ [iffalseInstructions] ++ iftrueLabels ++ iffalseLabels
    
    in (conditionInstructions ++ [JumpIfTrue updatedCondCount, JumpIfFalse (updatedCondCount + 1)], finalIftrueLabels, iftrueCount)



generateInstructions (While condition body) conditionStack count =
    let (conditionInstructions, updatedConditionStack, updatedConditionCount) = generateInstructions condition conditionStack count

        foldlFunc (accInstructions, accStack, accCount) expr = 
            let (instructions, stack, newCount) = generateInstructions expr updatedConditionStack accCount
            in (accInstructions ++ instructions, accStack ++ stack, newCount)

        (bodyInstructions, newLabels, finalCount) = foldl foldlFunc ([], [], (updatedConditionCount + 2)) body

        finalConditionStack = updatedConditionStack ++ [conditionInstructions ++ [JumpIfTrue (updatedConditionCount + 1)]] ++ [bodyInstructions ++ [Loop (updatedConditionCount)]] ++ newLabels

    in ([Loop updatedConditionCount], finalConditionStack, finalCount)


frameInstructions :: [AST] -> [[Instructions]] -> [[Instructions]] -> Int -> ([[Instructions]], [[Instructions]], Int)
frameInstructions asts _ conditions2 count =
    let foldlFunc (accInstructions1, accInstructions2, accCount) ast = 
            let (newInstructions1, newInstructions2, newCount) = generateInstructions ast conditions2 accCount
            in (accInstructions1 ++ [newInstructions1], accInstructions2 ++ newInstructions2, newCount)
        (finalInstructions1, finalInstructions2, finalCount) = foldl foldlFunc ([], [], count) asts
    in (finalInstructions1, finalInstructions2, finalCount)

parseArguments :: [String] -> Maybe (Bool, String)
parseArguments ["-dasm", file] = Just (True, file)
parseArguments [file] = Just (False, file)
parseArguments _ = Nothing

maybeToString :: Maybe String -> String
maybeToString Nothing = []
maybeToString (Just a) = a

main :: IO ()
main = do
    args <- getArgs

    case parseArguments args of
        Just (dasmFlag, fileArg) -> do
            maybeContents <- loadCodeFile fileArg
            librariesMaybeContents <- loadCodeFile "System.hpr"

            let ast = fileRead (Just (maybeToString librariesMaybeContents ++ "\n\n" ++ maybeToString maybeContents))
            case ast of
                [ASTError str] -> do
                    putStrLn str
                    exitWith (ExitFailure 84)
                _ -> do
                    let (frames, labels, _) = frameInstructions ast [] [] 0
                    let instructions = last frames
                    let env = (init frames, labels, instructions)
                    writeBytecodeFile "generator.out" env
                    if dasmFlag then displayBytecode env else return ()

        Nothing -> do
            putStrLn "Invalid command line arguments. Usage: ./psc [-dasm] <file>"
            exitWith (ExitFailure 1)
