module ReadTMcode (readProgramFromFile) where

import Types
import Text.Regex
import Control.Monad

readProgramFromFile fileName = do
    listOfLines <- getLines fileName
    return $ linesToProgram listOfLines 0
    where
        getLines = liftM lines . readFile

linesToProgram [] _ = []
linesToProgram (x:xs) i =
    if null (readInstruction x i) 
        then
            linesToProgram xs i
        else
            (readInstruction x i) ++ (linesToProgram xs (i+1))


readInstruction str i
    | matchRegex roRegex str /= Nothing =
        let (Just subStrs) = matchRegex roRegex str
            addr = read (subStrs !! 0)
        in
            if i == addr then 
                let roi = strToROInstruction (subStrs !! 1)
                    r = read (subStrs !! 2)
                    s = read (subStrs !! 3)
                    t = read (subStrs !! 4)
                    comment = subStrs !! 5
                in [(RO roi r s t comment)]
            else
                error ("Instructions out of order: expected instruction " ++ (show i) ++ ", got instruction " ++ (show addr) ++ " in string:\n" ++ str ++ "\n")
     | matchRegex rmRegex str /= Nothing =
        let (Just subStrs) = matchRegex rmRegex str
            addr = read (subStrs !! 0)
        in 
            if i == addr then
                let rmi = strToRMInstruction (subStrs !! 1)
                    r = read (subStrs !! 2)
                    d = read (subStrs !! 3)
                    s = read (subStrs !! 4)
                    comment = subStrs !! 5
                in [(RM rmi r d s comment)]
            else 
                error ("Instructions out of order: expected instruction " ++ (show i) ++ ", got instruction " ++ (show addr) ++ " in string:\n" ++ str ++ "\n")
    | matchRegex (mkRegex "[*](.*)") str /= Nothing = []
    | otherwise =
        error ("Failed to read in instruction at address " ++ (show i) ++ 
               " from string:\n" ++ str ++ "\n")
    
roRegex = mkRegex "[\t ]*([0-9]+):[\t ]*([A-Z]+)[\t ]*([0-9]+),([0-9]+),([0-9]+)[\t ]+(.*)"
rmRegex = mkRegex "[\t ]*([0-9]+):[\t ]*([A-Z]+)[\t ]*([0-9]+),[\t ]*(-?[0-9]+)[(]{1,1}([0-9]+)[)]{1,1}[\t ]+(.*)"
             
strToROInstruction str =
    case str of  
        "HALT"  -> HALT
        "IN"    -> IN
        "OUT"   -> OUT
        "ADD"   -> ADD
        "SUB"   -> SUB
        "MUL"   -> MUL
        "DIV"   -> DIV
        otherwise -> error ("String " ++ (show str) ++ "is not a valid RO instruction\n")
    
strToRMInstruction str = 
    case str of
        "LD"    -> LD
        "LDA"   -> LDA
        "LDC"   -> LDC
        "ST"    -> ST
        "JLT"   -> JLT
        "JLE"   -> JLE
        "JGE"   -> JGE
        "JGT"   -> JGT
        "JEQ"   -> JEQ
        "JNE"   -> JNE
        otherwise -> error ("String " ++ (show str) ++ "is not a valid RM instruction\n")
