--
-- EPITECH PROJECT, 2023
-- B-FUN-500-COT-5-2-glados-aman.menda [WSL: Ubuntu-22.04]
-- File description:
-- Syscalls
--

module VM.Syscalls (module VM.Syscalls) where

import qualified Data.Map as Map
import VM.Types

myabs :: Program
myabs = [LoadArg 0, PushValue (Integer 0), PushValue (Op Less), SysCall, JumpIfTrue 0, JumpIfFalse 1]

defaultEnv :: Env
defaultEnv = Map.empty

addInEnv :: String -> Value -> Env -> Env
addInEnv s v env = Map.insert s v env