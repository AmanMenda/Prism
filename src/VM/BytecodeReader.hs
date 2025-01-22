--
-- EPITECH PROJECT, 2024
-- B-FUN-500-COT-5-2-glados-aman.menda
-- File description:
-- BytecodeReader
--

module VM.BytecodeReader (module VM.BytecodeReader) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import Control.Monad (replicateM)
-- import Debug.Trace (trace)
import Data.Binary.Get
import Data.String

-- import Compiler.CompilerTypes
import VM.Types

-- PRISM = 0x505249534D with ascii table

deserializeBuiltins :: Get Builtins
deserializeBuiltins = do
    opCode <- getWord8
    case opCode of
        0x00 -> return Add
        0x01 -> return Sub
        0x02 -> return Mul
        0x03 -> return Div
        0x04 -> return Eq
        0x05 -> return Less
        _    -> error "Invalid opcode for Builtins"

deserializeValue :: Get Value
deserializeValue = do
    tag <- getWord8
    case tag of
        0x06 -> Integer . fromIntegral <$> getInt32be
        0x07 -> do
            b <- getWord8
            case b of
                0 -> return (Boolean False)
                1 -> return (Boolean True)
                _ -> error "Invalid boolean value"
        0x08 -> Op <$> deserializeBuiltins
        0x09 -> Function <$> deserializeInstructions
        0x0A -> do
            strLen <- fromIntegral <$> getInt32be
            Symbol <$> (fromString . BC.unpack <$> getByteString strLen)
        _    -> error "Invalid tag for Value"

deserializeInstruction :: Get Instructions
deserializeInstruction = do
    tag <- getWord8
    case tag of
        0x0B -> return SysCall
        0x0C -> return RetValue
        0x0D -> PushArg <$> deserializeValue
        0x0E -> PushValue <$> deserializeValue
        0x0F -> PushFunc . fromIntegral <$> getWord32be
        0x10 -> Loop . fromIntegral <$> getWord32be
        0x11 -> LoadArg . fromIntegral <$> getWord32be
        0x12 -> do
            strLen <- fromIntegral <$> getInt32be
            LoadName <$> (fromString . BC.unpack <$> getByteString strLen)
        0x13 -> do
            strLen <- fromIntegral <$> getInt32be
            StoreValue <$> (fromString . BC.unpack <$> getByteString strLen)
        0x14 -> JumpIfTrue . fromIntegral <$> getWord32be
        0x15 -> JumpIfFalse . fromIntegral <$> getWord32be
        _    -> error "Invalid tag for Instructions"

deserializeInstructions :: Get [Instructions]
deserializeInstructions = do
    len <- fromIntegral <$> getWord32be
    replicateM len deserializeInstruction

deserializeInstructionsList :: Get [[Instructions]]
deserializeInstructionsList = do
    len <- fromIntegral <$> getWord32be
    replicateM len deserializeInstructions

deserializeCode :: Get Code
deserializeCode = do
    frames <- deserializeInstructionsList
    labels <- deserializeInstructionsList
    main <- deserializeInstructions
    return (frames, labels, main)

deserializeHeader :: Get ()
deserializeHeader = do
    magicNumber <- getWord64be
    if magicNumber /= 0x505249534D -- this magicNumber identifies the bytecode file as an Prism Bytecode File
        then error "Invalid Magic Number - It isn't a Prism bytecode file"
        else do
            _ <- getFloatbe
            -- version <- getFloatbe
            -- trace ("Prism Version : " ++ show version ++ "\n") $ pure ()
            return ()

readBytecodeFile :: FilePath -> IO Code
readBytecodeFile path = do
    contents <- BS.readFile path
    let deserializedData = deserializeHeader >> deserializeCode
    case runGetOrFail deserializedData (BS.fromStrict contents) of
        Left (_, _, errMsg) -> error errMsg
        Right (_, _, code) -> return code

-- main :: IO ()
-- main = do
--     loadedCode <- readBytecodeFile "generator.out"
--     print loadedCode
