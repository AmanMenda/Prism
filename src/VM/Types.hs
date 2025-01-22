--
-- EPITECH PROJECT, 2023
-- B-FUN-500-COT-5-2-glados-aman.menda [WSL: Ubuntu-22.04]
-- File description:
-- Types
--

module VM.Types (module VM.Types) where

import qualified Data.Map as Map

data Value = Integer Int | Boolean Bool | Op Builtins | Function Program | Symbol String
    deriving (Show, Ord, Eq)

data Builtins =  Add | Sub  | Mul | Div
              |  Eq  | Less | Sup
    deriving (Show, Ord, Eq)

data Instructions = PushValue Value         -- add new value on top of the stack
                |   SysCall                 -- apply the builtin on the stack
                |   RetValue                -- returns the value on top of the stack and terminate the execution
                |   JumpIfFalse Int         -- add instructions from the Label when the condition is false
                |   JumpIfTrue Int          -- add instructions from the Label when the condition is true
                |   PushArg Value           -- push the value into the queue of args
                |   StoreValue String       -- add the name in the environment
                |   LoadName String         -- push in the stack the value of the name
                |   LoadArg Int             -- find the index in the queue and push it in the stack
                |   PushFunc Int            -- add the list of instruction according to the position of th function in the frame
                |   Loop Int                -- add instructions from Label when the condition of the loop is true 

    deriving (Show, Ord, Eq)

type Stack = [Value]

type Program = [Instructions]

type Args = [Value]

type Env = Map.Map String Value

type Code = ([Program], [Program], Program)
