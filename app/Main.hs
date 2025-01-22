import VM.BytecodeReader
import VM.Types
import VM.VirtualMachine
import VM.Syscalls

codeOutput :: Either String Value -> IO ()
codeOutput (Right (Integer i)) = print i
codeOutput (Right (Boolean b)) = print b
codeOutput (Left str) = print str
codeOutput _ = print "This case is not supported !"

getInstructions :: Code -> Program
getInstructions (_, _, instructions) = instructions

main :: IO ()
main = do
    loadedCode <- readBytecodeFile "generator.out"
    let output = exec [] (getInstructions loadedCode) [] defaultEnv loadedCode
    codeOutput output
