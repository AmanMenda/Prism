module BytecodeTranslator.BytecodePrinter (module BytecodeTranslator.BytecodePrinter) where

import VM.Types

displayFunctionValue :: [Instructions] -> [String]
displayFunctionValue [] = []
displayFunctionValue (x:xs) = ("\t\t  " ++ displayInstruction x) : displayFunctionValue xs

displayValue :: Value -> String
displayValue (Integer i)   = show i
displayValue (Boolean b)   = show b
displayValue (Op op)       = "<builtin> " ++ show op
displayValue (Function fn) = "<func> :\n" ++ init (unlines (displayFunctionValue fn))
displayValue (Symbol str)  = show str

displayInstruction :: Instructions -> String
displayInstruction SysCall            = "SysCall"
displayInstruction RetValue           = "RetValue"
displayInstruction (PushArg value)    = "PushArg       " ++ displayValue value
displayInstruction (PushValue value)  = "PushValue     " ++ displayValue value
displayInstruction (PushFunc pos)     = "PushFunc      #" ++ show pos
displayInstruction (Loop pos)         = "Loop          #" ++ show pos
displayInstruction (LoadArg pos)      = "LoadArg       #" ++ show pos
displayInstruction (LoadName str)     = "LoadName      " ++ show str
displayInstruction (StoreValue str)   = "StoreValue    " ++ show str
displayInstruction (JumpIfTrue pos)   = "JumpIfTrue    #" ++ show pos
displayInstruction (JumpIfFalse pos)  = "JumpIfFalse   #" ++ show pos

displayInstructions :: [Instructions] -> String
displayInstructions = unlines . map displayInstruction

displayInstructionsList :: String -> Int -> [[Instructions]] -> IO ()
displayInstructionsList _ _ [] = return ()
displayInstructionsList _ _ [[]] = return ()
displayInstructionsList str i (x:xs) = do
    putStrLn ("\n<" ++ str ++ " " ++ show i ++ ">")
    putStr $ displayInstructions x
    displayInstructionsList str (i + 1) xs

displayBytecode :: Code -> IO ()
displayBytecode (frame, labels, main) = do
    putStrLn ("Prism Version : " ++ "1.0" ++ "\n")
    putStrLn "  -- FRAME"
    displayInstructionsList "frame" 0 frame
    putStrLn "\n  -- LABELS"
    displayInstructionsList "label" 0 labels
    putStrLn "\n  -- MAIN INSTRUCTIONS\n"
    putStr $ displayInstructions main

-- main :: IO ()
-- main = do
--     let sampleData = ([[PushValue (Integer 5), PushValue (Boolean True), PushValue (Op Add), PushValue (Function [JumpIfFalse 5, SysCall, RetValue]), PushValue (Symbol "name")], [JumpIfTrue 14, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "bobo", LoadName "stella", RetValue]], [[PushArg (Integer 5), PushArg (Boolean True), PushArg (Op Add), PushArg (Function [JumpIfFalse 5, SysCall, RetValue]), PushArg (Symbol "name")], [LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "sinon", LoadName "etiope", RetValue]], [LoadArg 520, PushFunc 63051, Loop 500000085, RetValue])
--     displayBytecode sampleData
