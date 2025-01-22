--
-- EPITECH PROJECT, 2023
-- VM
-- File description:
-- virtual machine
--

module VM.VirtualMachine (module VM.VirtualMachine) where

import qualified Data.Map as Map
import Data.Maybe()
import VM.Types
import VM.Syscalls

myFunc :: Program
myFunc = [LoadArg 1, LoadArg 0, PushValue (Op Div), SysCall, RetValue]

getSymbolValue :: String -> Env -> Either String Value
getSymbolValue s env = case Map.lookup s env of
  Just v -> Right v
  Nothing -> Left "The symbol is not define"

execCall :: Builtins -> Stack -> Either String Stack
execCall Eq (a:b:xs) = Right $ Boolean (a == b):xs
execCall Less (a:b:xs) = Right $ Boolean (a < b):xs
execCall builtin (Integer a:Integer b:xs) = do
  case builtin of
    Add -> Right $ Integer (a + b):xs
    Sub -> Right $ Integer (a - b):xs
    Mul -> Right $ Integer (a * b):xs
    Div -> case b of
        0 -> Left "# execCall => Error: division by 0"
        _ -> Right $ Integer (a `div` b):xs
execCall _ _ = Left "# execCall => Error: Builtin need 2 arguments"

exec :: Args -> Program -> Stack -> Env -> Code -> Either String Value
exec args ((PushValue x):xs) stack env todo = case x of
  Symbol name ->  case result of
                           Left err -> Left err
                           Right s -> exec args xs (s:stack) env todo
                           where result = getSymbolValue name env
  oc -> exec args xs (oc:stack) env todo
exec args ((LoadName name):xs) stack env todo = case result of
  Left err -> Left err
  Right s -> exec args xs (s:stack) env todo
  where result = getSymbolValue name env
exec args ((StoreValue name):xs) (v:stack) env todo = exec args xs stack (addInEnv name v env) todo
exec args (SysCall:zs) (Op x:stack) env todo = do
  result <- execCall x stack
  exec args zs result env todo
exec args (SysCall:insts) ((Function instsFunc):stack) env todo = do
  res <- exec args instsFunc [] env todo
  exec args insts (res:stack) env todo
exec args ((JumpIfFalse n):insts) (Boolean b:remaining) env (frame, label, instructions) | b == False = exec args ((label!!n) ++ insts) remaining env (frame, label, instructions)
                                                                 | otherwise = exec args insts (Boolean b:remaining) env (frame, label, instructions)
exec args ((JumpIfTrue n):insts) (Boolean b:remaining) env (frame, label, instructions) | b == True = exec args ((label!!n) ++ insts) remaining env (frame, label, instructions)
                                                                | otherwise = exec args insts (Boolean b:remaining) env (frame, label, instructions)

exec args ((Loop n):insts) stack env (frame, label, instructions) = exec args ((label!!n) ++ insts) stack env (frame, label, instructions)
exec args ((PushFunc n):insts) stack env (frame, label, instructions) = exec args insts ((Function (frame!!n)):stack) env (frame, label, instructions)
exec args ((PushArg n):insts) stack env todo = case n of
  Symbol name ->  case result of
                           Left err -> Left err
                           Right s -> exec (s:args) insts stack env todo
                           where result = getSymbolValue name env
  oc -> exec (oc:args) insts stack env todo
exec args ((LoadArg n):insts) stack env todo = exec args insts ((args!!n):stack) env todo
exec _ (RetValue:_) (x:_) _ _ = Right x
exec _ _ _ _ _         = Left "# exec: runtime error"