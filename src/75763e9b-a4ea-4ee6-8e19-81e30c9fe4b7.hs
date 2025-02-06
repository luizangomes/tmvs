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

module Main where

import System.Environment(getArgs)
import System.IO
import Data.Array
import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import Text.Regex

import Types
import VirtualMachine
import Constants
import ReadTMcode
import GUIsetupWidgets
import GUItypes
import GUIdrawing
---------
----TODO:
-- add a seperate prepass to check reg numbers are inbounds, compile time

---------            

main :: IO ()
main = do
   
    args <- getArgs
    if (length args) /= 2 
        then 
            error "Incorrect number of parameters. Parameters required are 1) filename of TM instruction file and 2) dmem maximum address.\n"
        else
            return ()
    tmCodeFile <- return $ ((args !! 0) :: String)
    dmemMaxAddr <- return $ (read (args !! 1) :: Int)
 
    program <- readProgramFromFile tmCodeFile
    
    stateListRef <- initTMstateList program dmemMaxAddr
    ioLogRef <- ((newIORef []) :: IO (IORef [LogEntry])) 

    initGUI
    (mainWindow, leftVbox, rightPaned) <- initWindow tmCodeFile
    (stackVbox, 
     stackScrWin,
     stackTable,
     stackEntryArray,
     stackArrowArray, 
     stackFollowCheckButton, 
     stackEditableCheckButton) <- initStackWidgets dmemMaxAddr
    (instVbox, 
     instScrWin, 
     instTable, 
     instEntryArray, 
     instArrowArray, 
     instFollowCheckButton) <- initInstWidgets stateListRef
    (refFrame, regTable, regEntries) <- initRegWidgets
    (controlFrame, 
     stepBackXButton, 
     stepBackButton, 
     stepButton, 
     stepXButton, 
     stepEndButton, 
     stepBackXEntry, 
     stepXEntry,
     currentStepLabel,
     stepResultLabel) <- initControlWidgets
    (outputFrame, outputScrWin, outputLabel) <- initOutputWidgets

    boxPackStart leftVbox controlFrame PackNatural 0
    boxPackStart leftVbox refFrame PackNatural 0  
    boxPackStart leftVbox outputFrame PackGrow 0

    panedAdd1 rightPaned stackVbox
    panedAdd2 rightPaned instVbox
 
    stackWidgets <- return (StackWidgets stackScrWin 
                                         stackTable 
                                         stackEntryArray 
                                         stackArrowArray 
                                         stackFollowCheckButton 
                                         stackEditableCheckButton)
    instWidgets <- return (InstWidgets instScrWin 
                                       instTable 
                                       instEntryArray 
                                       instArrowArray 
                                       instFollowCheckButton)
    regWidgets <- return (RegWidgets regTable regEntries)
    controlWidgets <- return (ControlWidgets stepBackXButton 
                                             stepBackButton 
                                             stepButton
                                             stepXButton
                                             stepEndButton
                                             stepBackXEntry
                                             stepXEntry
                                             currentStepLabel
                                             stepResultLabel)
    outputWidgets <- return (OutputWidgets outputScrWin outputLabel)

   
    gui <- return (GUI stackWidgets 
                       instWidgets 
                       regWidgets 
                       controlWidgets 
                       outputWidgets)
                       
    sim <- return (Sim stateListRef ioLogRef gui)


    onClicked stepBackXButton (retrieveXandPerformXStepsBack sim)
    onClicked stepBackButton (simulateXstepsBack 1 sim)
    onClicked stepButton (simulateXstepsForwards 1 sim)
    onClicked stepXButton (retrieveXandPerformXStepsForward sim)
    onClicked stepEndButton (simulateXstepsForwards 100000 sim)
    

    onClicked instFollowCheckButton (updateInstScrWinSlider sim)
    onClicked stackFollowCheckButton (updateStackScrWinSlider sim)

    onDestroy mainWindow mainQuit
    widgetShowAll mainWindow
    

    drawCurrentState sim
    mainGUI

-- Init State

initTMstateList program dmemMaxAddr = do
    initState <- return (initTMstate program dmemMaxAddr)
    stateListRef <- newIORef [initState]
    return stateListRef

-- Simulate Backwards Functions

retrieveXandPerformXStepsBack sim@(Sim tmStateListRef _ gui) = do
    controlWidgets <- return $ getControlWidgets gui
    stepBackXEntry <- return $ getStepBackXEntry controlWidgets
    result <- getAndCheckIntFromEntry stepBackXEntry
    if result == Nothing
        then 
            return ()
        else let (Just numSteps) = result
             in do
                tmStateList <- readIORef tmStateListRef
                (TMstate currentStep _ _ _ _ _) <- return $ head tmStateList
                simulateXstepsBack (min numSteps currentStep) sim

simulateXstepsBack x sim@(Sim tmStateListRef ioLogRef gui) = do
    tmStateList <- readIORef tmStateListRef
    (TMstate currentStep _ _ _ _ _) <- return $ head tmStateList
    writeIORef tmStateListRef (drop x tmStateList)

    ioLog <- readIORef ioLogRef
    writeIORef ioLogRef (removeIOlogEntries (currentStep-x) ioLog)

    drawCurrentState sim

    where 
        removeIOlogEntries to ioLog
            | null ioLog = []
            | (fst (head ioLog)) > to = removeIOlogEntries to (tail ioLog)
            | otherwise = ioLog
         
-- Simulate Forwards Functions
       
retrieveXandPerformXStepsForward sim@(Sim tmStateListRef _ gui) = do
    controlWidgets <- return $ getControlWidgets gui
    stepXEntry <- return $ getStepXEntry controlWidgets
    result <- getAndCheckIntFromEntry stepXEntry
    if result == Nothing 
        then
            return ()
        else let (Just numSteps) = result
             in simulateXstepsForwards numSteps sim

simulateXstepsForwards x sim@(Sim tmStateListRef ioLogRef gui) = do
    performXtmSteps x tmStateListRef ioLogRef
    drawCurrentState sim

    where 
        performXtmSteps x tmStateListRef ioLogRef
            | x==0 = return ()
            | otherwise = do
                tmStateList <- readIORef tmStateListRef
                currentState <- return $ head tmStateList
                newState@(TMstate step imem dmem dmemInfo regs result) <- return $ performTMstep currentState
                modifyIORef tmStateListRef (addToHead newState)
                checkPCisInImem regs imem
                case result of
                    TMokay -> performXtmSteps (x-1) tmStateListRef ioLogRef
                    (TMoutput val) -> do
                        modifyIORef ioLogRef (addToHead (step,"OUT: " ++ (show val)))
                        performXtmSteps (x-1) tmStateListRef ioLogRef
                    (TMinputReq _) -> getInputAndUpdateTMstate sim
                    otherwise -> return ()

-- Dialog Box GUI Functions

getAndCheckIntFromEntry entry = do
    text <- entryGetText entry
    regexResult <- return $ matchRegex (mkRegex "^([0-9]+)$") text
    if (regexResult == Nothing) 
        then do
            dia <- dialogNew
            dialogAddButton dia stockClose ResponseClose
            upbox <- dialogGetUpper dia
            errorLabel <- labelNew (Just "Enter an intger in this field.")
            boxPackStart upbox errorLabel PackGrow 10
            widgetShowAll upbox
            dialogRun dia
            widgetDestroy dia
            return Nothing
        else let (Just subStrs) = regexResult
             in return (Just (read (head subStrs) :: Int))

getInputAndUpdateTMstate sim@(Sim tmStateListRef ioLogRef gui) = do
    dia <- dialogNew
    dialogAddButton dia stockApply ResponseApply
    upbox <- dialogGetUpper dia
    infoLabel <- labelNew (Just "TM machine requests input (integer):")
    entry <- entryNew
    boxPackStart upbox infoLabel PackNatural 10
    boxPackStart upbox entry PackNatural 10
    widgetShowAll upbox
    dialogRun dia
    result <- getAndCheckIntFromEntry entry 
    widgetDestroy dia
    if (result == Nothing)
        then
            getInputAndUpdateTMstate sim
        else
            let (Just val) = result
            in do 
                tmStateList <- readIORef tmStateListRef
                (TMstate step imem dmem dmemInfo regs stepResult) <- return $ head tmStateList
                (TMinputReq inputReg) <- return $ stepResult

                newRegs <- return $ regs // [(inputReg,val)]
                newState <- return (TMstate step imem dmem dmemInfo newRegs stepResult)
                writeIORef tmStateListRef (newState:(tail tmStateList))

                logList <- readIORef ioLogRef
                logStr <- return ("IN: val " ++ (show val) ++ " -> reg " ++ (show inputReg))
                modifyIORef ioLogRef (addToHead (step,logStr))
                
                    
-- Helpers

checkPCisInImem regs imem = 
    if ( (regs!pc) <= (snd (bounds imem)) && (regs!pc) >= (fst (bounds imem)) )
        then
            return ()
        else
            error ("pc (val: " ++ (show (regs!pc)) ++ ") is outside imem bounds " ++ (show (bounds imem)))

addToHead e list = e:list 

     
        


    

     



