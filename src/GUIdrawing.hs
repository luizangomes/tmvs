module GUIdrawing where

import Data.IORef
import Graphics.UI.Gtk
import Data.Array.IO
import Array
import Text.Printf

import GUItypes

import VirtualMachine
import IOArrayHelpers
import Constants


-- Draw current state

drawCurrentState simState@(Sim tmStateListRef ioLogRef gui) = do
    tmStateList <- readIORef tmStateListRef
    currentState <- return $ head tmStateList

    regs <- return $ getTMstateRegs currentState
    imem <- return $ getTMstateimem currentState
    dmem <- return $ getTMstatedmem currentState
    dmemInfo <- return $ getTMstatedmemInfo currentState
    step <- return $ getTMstateStep currentState
    stepResult <- return $ getTMstateStepResult currentState

    dmemMaxAddr <- return $ snd (bounds dmem)
    imemMaxAddr <- return $ snd (bounds imem)

    checkRegsInRangeForDisplay regs dmemMaxAddr imemMaxAddr

    -- update imem widgets
    setPCarrow (regs!pc) gui
    updateInstScrWinSlider simState

    -- update dmem widgets
    setdmemText dmem dmemInfo gui
    setFParrow (dmemMaxAddr-(regs!fp)) gui
    updateStackScrWinSlider simState

    setIOlogText ioLogRef gui

    setRegsText regs gui

    setControlInfo step stepResult gui

    setControlButtonAccess step stepResult gui


-- Safety checks

checkRegsInRangeForDisplay regs dmemMaxAddr imemMaxAddr
    | (regs!pc) < 0 || (regs!pc) > imemMaxAddr = error ("PC (val: " ++ (show (regs!pc)) ++ ") is out of simulator display range - imem range is (0," ++ (show imemMaxAddr) ++ ")\n")
    | (regs!fp) < 0 || (regs!fp) > dmemMaxAddr = error ("FP (val: " ++ (show (regs!fp)) ++ ") is out of simulator display range - dmem range is (0," ++ (show dmemMaxAddr) ++ ")\n")
    | otherwise = return ()
    

-- Set Control Properties

setControlButtonAccess step stepResult gui = do
    controlWidgets <- return $ getControlWidgets gui
    stepBackXButton <- return $ getStepBackXButton controlWidgets
    stepBackButton <- return $ getStepBackButton controlWidgets
    stepButton <- return $ getStepButton controlWidgets
    stepXButton <- return $ getStepXButton controlWidgets
    stepEndButton <- return $ getStepEndButton controlWidgets
    
    stepBackSensitive <- return (step > 0)
    widgetSetSensitive stepBackXButton stepBackSensitive
    widgetSetSensitive stepBackButton stepBackSensitive

    stepForwardSensitive <- return (not (tmHaltedOrError stepResult))
    widgetSetSensitive stepButton stepForwardSensitive
    widgetSetSensitive stepXButton stepForwardSensitive
    widgetSetSensitive stepEndButton stepForwardSensitive

setControlInfo step stepResult gui = do
    controlWidgets <- return $ getControlWidgets gui
    currentStepLabel <- return $ getCurrentStepLabel controlWidgets
    stepResultLabel <- return $ getStepResultLabel controlWidgets
    labelSetText currentStepLabel (show step)
    labelSetText stepResultLabel (show stepResult)

-- Set IO Log Properties

setIOlogText ioLogRef gui = do
    ioLog <- readIORef ioLogRef

    outputWidgets <- return $ getOutputWidgets gui
    outputLabel <- return $ getOutputLabel outputWidgets
   
    labelSetText outputLabel (buildLogStr (reverse ioLog))

    where
        buildLogStr ioLog
            | null ioLog = ""
            | otherwise = let logEntry = head ioLog
                          in (printf "%4d: " (fst logEntry)) ++ 
                             (snd logEntry) ++ "\n" ++ 
                             (buildLogStr (tail ioLog))

-- Set Instruction Memory Properties

setPCarrow pc gui = do
    instWidgets <- return $ getInstWidgets gui
    instArrowArray <- return $ getInstArrows instWidgets
    (_,arrowArraySize) <- getBounds instArrowArray
    hideAllWidgetsInArray instArrowArray arrowArraySize
    currentPCarrow <- readArray instArrowArray pc
    widgetShow currentPCarrow

updateInstScrWinSlider simState@(Sim tmStateListRef _ gui) = do
    follow <- getInstFollowState gui
    if follow 
        then do
            tmStateList <- readIORef tmStateListRef
            currentState <- return $ head tmStateList
            regs <- return $ getTMstateRegs currentState
            imem <- return $ getTMstateimem currentState
            instWidgets <- return $ getInstWidgets gui
            instScrWin <- return $ getInstScrWin instWidgets
            instScrWinVAdjustment <- scrolledWindowGetVAdjustment instScrWin
            setSlider (regs!pc) ((snd (bounds imem))+1) instScrWinVAdjustment         
        else
            return ()
    where
        getInstFollowState gui = do
            instWidgets <- return $ getInstWidgets gui
            instFollowCheckButton <- return $ getInstFollowCheckButton instWidgets
            toggleButtonGetActive instFollowCheckButton 

-- Set Data Memory Properties
    
setFParrow fp gui = do
    stackWidgets <- return $ getStackWidgets gui
    stackArrowArray <- return $ getStackArrows stackWidgets
    (_,arrowArraySize) <- getBounds stackArrowArray
    hideAllWidgetsInArray stackArrowArray arrowArraySize
    currentFParrow <- readArray stackArrowArray fp
    widgetShow currentFParrow

setdmemText dmem dmemInfo gui = do
    stackWidgets <- return $ getStackWidgets gui
    stackEntryArray <- return $ getStackEntries stackWidgets
    setdmemTextHelper dmem dmemInfo 0 stackEntryArray
    where    
        setdmemTextHelper dmem dmemInfo i entries
            | i > maxdmemAddr = return ()
            | otherwise = do
                addrEntry <- ix2d entries i 0
                entrySetText addrEntry (show currentAddr)
                memEntry <- ix2d entries i 1
                entrySetText memEntry (show (dmem!currentAddr))        
                funEntry <- ix2d entries i 2        
                entrySetText funEntry (fst (dmemInfo!currentAddr))
                varEntry <- ix2d entries i 3
                entrySetText varEntry (snd (dmemInfo!currentAddr))
                setdmemTextHelper dmem dmemInfo (i+1) entries
            where currentAddr = (maxdmemAddr-i)
                  maxdmemAddr = snd (bounds dmem)

updateStackScrWinSlider simState@(Sim tmStateListRef _ gui) = do
    follow <- getStackFollowState gui
    if follow 
        then do
            tmStateList <- readIORef tmStateListRef
            currentState <- return $ head tmStateList
            regs <- return $ getTMstateRegs currentState
            dmem <- return $ getTMstatedmem currentState
            stackWidgets <- return $ getStackWidgets gui
            stackScrWin <- return $ getStackScrWin stackWidgets
            stackScrWinVAdjustment <- scrolledWindowGetVAdjustment stackScrWin
            dmemMaxAddr <- return (snd (bounds dmem))
            setSlider (dmemMaxAddr-(regs!fp)) (dmemMaxAddr+1) stackScrWinVAdjustment         
        else
            return ()
    where
        getStackFollowState gui = do
            stackWidgets <- return $ getStackWidgets gui
            stackFollowCheckButton <- return $ getStackFollowCheckButton stackWidgets
            toggleButtonGetActive stackFollowCheckButton     

-- Set Registers Properties

setRegsText regs gui = do
    regWidgets <- return $ getRegWidgets gui
    regEntryArray <- return $ getRegEntries regWidgets
    setRegsTextHelper regs regEntryArray 0
    where 
        setRegsTextHelper regs regEntryArray i 
            | i>7 = return ()
            | otherwise = do
                entry <- readArray regEntryArray i
                entrySetText entry (show (regs!i))
                setRegsTextHelper regs regEntryArray (i+1)

-- Drawing helpers

setSlider topFrac bottomFrac adjustment = do
    upperSliderValue <- adjustmentGetUpper adjustment
    pageSize <- adjustmentGetPageSize adjustment
    newPosition <- return ((((fromIntegral topFrac)
                            /(fromIntegral bottomFrac))
                           *upperSliderValue)
                          -(pageSize/2.0))
    if newPosition < 0.0 
        then
            adjustmentSetValue adjustment 0.0
        else
            if newPosition > (upperSliderValue-pageSize)
                then
                adjustmentSetValue adjustment (upperSliderValue-pageSize)
            else
                adjustmentSetValue adjustment newPosition 

hideEntryRow entries i = do
    (_,jSize) <- dim2d entries
    hideEntryRowHelper entries i jSize

hideEntryRowHelper entries i j
    | j < 0 = return ()
    | otherwise = do
        hideEntryInArray entries i j
        hideEntryRowHelper entries i (j-1)

hideEntryInArray entries i j = do
    entry <- ix2d entries i j
    widgetHide entry
    widgetQueueDraw entry


setColumnProperties arr col properties = do 
    (_,numRows) <- getBounds arr
    setColumnPropertiesHelper arr col properties numRows

setColumnPropertiesHelper _ _ _ (-1) = return ()
setColumnPropertiesHelper arr col properties i = do
    entry <- ix2d arr i col
    set entry properties
    setColumnPropertiesHelper arr col properties (i-1)


hideAllWidgetsInArray array i
    | i < 0 = return ()
    | otherwise = do
        widget <- readArray array i
        widgetHide widget
        hideAllWidgetsInArray array (i-1)
