module BytecodeTranslator.BytecodeGenerator (module BytecodeTranslator.BytecodeGenerator) where

import qualified Data.ByteString as BS
import Data.Binary.Put
import Data.String

import VM.Types

-- PRISM = 0x505249534D with ascii table

-- add a "NOTHING" instruction ???

-- putInt  : for signed integer
-- putWord : for unsigned integer

serializeBuiltins :: Builtins -> Put
serializeBuiltins op = putWord8 $ case op of
    Add  -> 0x00
    Sub  -> 0x01
    Mul  -> 0x02
    Div  -> 0x03
    Eq   -> 0x04
    Less -> 0x05

serializeValue :: Value -> Put
serializeValue val = case val of
    Integer i   -> putWord8 0x06 >> putInt32be (fromIntegral i)
    Boolean b   -> putWord8 0x07 >> putWord8 (if b then 1 else 0)
    Op op       -> putWord8 0x08 >> serializeBuiltins op
    Function fn -> putWord8 0x09 >> serializeInstructions fn
    Symbol str  -> 
        let encodedStr :: String
            encodedStr = fromString str
        in putWord8 0x0A >> putInt32be (fromIntegral (length encodedStr)) >> putByteString (fromString str)

serializeInstruction :: Instructions -> Put
serializeInstruction instr = case instr of
    SysCall         -> putWord8 0x0B
    RetValue        -> putWord8 0x0C
    PushArg v       -> putWord8 0x0D >> serializeValue v
    PushValue v     -> putWord8 0x0E >> serializeValue v
    PushFunc pos    -> putWord8 0X0F >> putWord32be (fromIntegral pos)
    Loop pos        -> putWord8 0X10 >> putWord32be (fromIntegral pos)
    LoadArg pos     -> putWord8 0X11 >> putWord32be (fromIntegral pos)
    LoadName str    ->  
        let encodedStr :: String
            encodedStr = fromString str
        in putWord8 0x12 >> putInt32be (fromIntegral (length encodedStr)) >> putByteString (fromString str)
    StoreValue str  -> 
        let encodedStr :: String
            encodedStr = fromString str
        in putWord8 0x13 >> putInt32be (fromIntegral (length encodedStr)) >> putByteString (fromString str)
    JumpIfTrue pos  -> putWord8 0X14 >> putWord32be (fromIntegral pos)
    JumpIfFalse pos -> putWord8 0x15 >> putWord32be (fromIntegral pos)

serializeInstructions :: [Instructions] -> Put
serializeInstructions list = do
    putWord32be (fromIntegral $ length list)
    mapM_ serializeInstruction list

serializeInstructionsList :: [[Instructions]] -> Put
serializeInstructionsList list = do
    putWord32be (fromIntegral $ length list)
    mapM_ serializeInstructions list

writeBytecodeFile :: FilePath -> Code -> IO ()
writeBytecodeFile path (frames, labels, main) = do
    let header = runPut $ do
            putWord64be 0x505249534D
            putFloatbe 1.0
    let body = runPut $ do
            serializeInstructionsList frames
            serializeInstructionsList labels
            serializeInstructions main
    BS.writeFile path $ BS.concat [BS.toStrict header, BS.toStrict body]

-- main :: IO ()
-- main = do
--     -- let sampleData = ([[JumpIfFalse 0, LoadArg 1], []], [[], []], [])
--     let sampleData = ([[LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue], [LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue]], [[LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue], [LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue]], [PushValue (Integer 5), StoreValue "a", PushValue (Integer 6), StoreValue "b", PushArg (Symbol "a"), PushArg (Symbol "b"), PushFunc 0, SysCall, StoreValue "c", PushValue (Integer 0), RetValue])

--     writeBytecodeFile "generator.out" sampleData

-- [
--     [[LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue], [LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue]],
--     [[LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue], [LoadArg 0, LoadArg 1, PushValue (Op Add), SysCall, StoreValue "c", LoadName "c", RetValue]],
--     [PushValue (Integer 5), StoreValue "a", PushValue (Integer 6), StoreValue "b", PushArg (Symbol "a"), PushArg (Symbol "b"), PushFunc 0, SysCall, StoreValue "c", PushValue (Integer 0), RetValue]
-- ]
