--Copyright 2010 David White

--This file is part of Tiny Machine Visual Simulator.

--Tiny Machine Visual Simulator is free software: you can redistribute 
--it and/or modify it under the terms of the GNU General Public License 
--as published by the Free Software Foundation, either version 3 of the 
--License, or (at your option) any later version.

--Tiny Machine Visual Simulator is distributed in the hope that it 
--will be useful, but WITHOUT ANY WARRANTY; without even the implied 
--warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--See the GNU General Public License for more details.

--You should have received a copy of the GNU General Public License
--along with Tiny Machine Visual Simulator.  If not, see 
-- <http://www.gnu.org/licenses/>.

module VirtualMachine where

import Text.Printf
import Text.Regex
import Control.Monad
import Data.Array
import Debug.Trace

import Types
import Constants
    
-- Virtual machine types

data TMstate = TMstate {
    getTMstateStep :: Int,
    getTMstateimem :: (Array Int Instruction), 
    getTMstatedmem :: (Array Int Int), 
    getTMstatedmemInfo :: (Array Int (String,String)),
    getTMstateRegs :: (Array Int Int),
    getTMstateStepResult :: StepResult }

data StepResult = TMokay
                | TMhalt 
                | TMinputReq Int
                | TMoutput Int 
                | TMiMemError Int
                | TMdMemError Int
                | TMzeroDivide
    deriving (Show)
-- hitting any type of error state does not change the pc

-- Printing functions

instance Show TMstate where
    show (TMstate step imem dmem dmemInfo regs result) = 
        ("Step: " ++ (show step) ++ "\n" ++ 
        "Step Result: " ++ (show result) ++ "\n" ++
        "Instruction Memory: \n" ++ (printInstructionArray imem 0) ++ "\n" ++
        "Data Memory: \n" ++ (show dmem) ++ "\n" ++
        "Data Memory additional info: \n" ++ (show dmemInfo) ++
        "Regs: \n" ++ (printRegs regs))   
        
printRegs :: (Array Int Int) -> String        
printRegs regs = 
    (printf "ac:  %4d    r4:  %4d\n" (regs!0) (regs!4)) ++
    (printf "ac1: %4d    fp:  %4d\n" (regs!1) (regs!5)) ++
    (printf "r2:  %4d    gp:  %4d\n" (regs!2) (regs!6)) ++
    (printf "r3:  %4d    pc:  %4d\n" (regs!3) (regs!7))
    
    
printInstructionArray :: Array Int Instruction -> Int -> String      
printInstructionArray instructions counter
    | counter > snd(bounds instructions) = ""
    | isROinstruction inst =
        do
            let (RO op r s t comment) = inst
            let lengthOfRegOperands = (length (show r)) + (length (show s)) + (length (show t))
            let spaces = genSpaces (13 - 2 - lengthOfRegOperands)
            let instructionString = printf "%3d:  %5s  %d,%d,%d%s\n" counter (show op) r s t (spaces ++ comment)
            instructionString ++ (printInstructionArray instructions (counter+1))
    | isRMinstruction inst =
        do
            let (RM op r d s comment) = inst            
            let instructionString = printf "%3d:  %5s  %d,%7d(%d) %s\n" counter (show op) r d s comment
            instructionString ++ printInstructionArray instructions (counter+1)
    where inst = instructions!counter
          genSpaces n
                | n<=0      = ""
                | otherwise = " " ++ (genSpaces (n-1))    
            
-- Manipulate Virtual Machine State

initTMstate program dmemMaxAddress = TMstate 0 imem dmemWithMaxAddress dmemInfo regs TMokay
    where
        imem = array (0,(length program)-1) [(ix,inst) | (ix,inst) <- zip [0..(length program)-1] program]
        dmem = array (0,dmemMaxAddress) [(i,0) | i <- [0..dmemMaxAddress]]
        dmemWithMaxAddress = dmem // [(0,dmemMaxAddress)]
        dmemInfo = array (0,dmemMaxAddress) [(i,("","")) | i <- [0..dmemMaxAddress]]
        regs = array (0,7) [(i,0) | i <- [0..7]]

performTMstep :: TMstate -> TMstate
performTMstep s0@(TMstate step imem dmem0 dmemInfo0 regs0 stepResult0)
    | tmHaltedOrError stepResult0 =
        error ("performTMstep called on halted or errored TM, TMstate follows:\n" ++ (show s0))
    | (not (inBounds (regs0!pc) imem)) = 
        (TMstate step imem dmem0 dmemInfo0 (regs0 // [(pc,0)]) (TMiMemError (regs0!pc)))
    | otherwise = 
        do
            let inst = imem!(regs0!pc)
            let (dmem1,regs1,stepResult1) = executeTMinstruction inst dmem0 (regs0 // [(pc,(regs0!pc)+1)])
            let dmemInfo1 = updatedmemInfo dmemInfo0 regs0 inst       
            if (tmHaltedOrError stepResult1)  
                then                     
                    (TMstate (step+1) imem dmem1 dmemInfo1 (regs1 // [(pc,0)]) stepResult1)
                else
                    if (inBounds (regs1!pc) imem)
                        then
                            (TMstate (step+1) imem dmem1 dmemInfo1 regs1 stepResult1)
                        else 
                            (TMstate (step+1) imem dmem1 dmemInfo1 (regs1 // [(pc,0)]) (TMiMemError (regs1!pc)))

executeTMinstruction inst@(RO roi r s t comment) dmem regs
    | roi == HALT   = (dmem,regs,TMhalt)
    | roi == IN     = (dmem,regs,TMinputReq r)
    | roi == OUT    = (dmem,regs,TMoutput (regs!r))
    | roi == ADD    = (dmem, regs // [(r, regs!s + regs!t)], TMokay)
    | roi == SUB    = (dmem, regs // [(r, regs!s - regs!t)], TMokay)
    | roi == MUL    = (dmem, regs // [(r, regs!s * regs!t)], TMokay)
    | roi == DIV    = if (regs!t == 0) then (dmem, regs, TMzeroDivide)
                                       else (dmem, regs // [(r, regs!s `div` regs!t)], TMokay)

executeTMinstruction inst@(RM rmi r d s comment) dmem regs
    | elem rmi [LD,ST] = 
        if a < fst (bounds dmem) || a > snd (bounds dmem) 
            then (dmem,regs,TMdMemError a)
            else case rmi of
                LD -> (dmem, regs // [(r, dmem!a)], TMokay)
                ST -> (dmem // [(a, regs!r)], regs, TMokay)
                      
    | rmi == LDA = (dmem, regs // [(r, a)], TMokay)
    
    | rmi == LDC = (dmem, regs // [(r, d)], TMokay)
    
    | elem rmi [JLT,JLE,JGE,JGT,JEQ,JNE] = 
        if test 
            then (dmem, regsNewPC, TMokay)
            else (dmem, regs, TMokay)
            
    where 
        a = d + regs!s
        regsNewPC = regs // [(pc, a)]
        test = case rmi of
            JLT -> regs!r < 0
            JLE -> regs!r <= 0
            JGE -> regs!r >= 0
            JGT -> regs!r > 0
            JEQ -> regs!r == 0
            JNE -> regs!r /= 0

updatedmemInfo dmemInfo regs (RM ST r d s comment) = 
    if (regexResult == Nothing) 
        then
            dmemInfo // [(a, ("",""))] 
        else 
            let (Just subStrs) = regexResult
            in 
                if (length subStrs /= 2)
                    then
                        dmemInfo // [(a, ("",""))]
                    else
                        dmemInfo // [(a, ((subStrs !! 0),(subStrs !! 1)))]
    where 
        a = d + regs!s
        regexResult = matchRegex infoRegex comment
        infoRegex = mkRegex ".*--(.*)--(.*)--"
updatedmemInfo dmemInfo _ _ = dmemInfo

-- Helpers

inBounds x arr = (x >= lower && x<= upper)
                 where (lower,upper) = bounds arr

tmHaltedOrError (TMhalt) = True
tmHaltedOrError (TMiMemError _) = True
tmHaltedOrError (TMdMemError _) = True
tmHaltedOrError (TMzeroDivide) = True
tmHaltedOrError otherwise = False


