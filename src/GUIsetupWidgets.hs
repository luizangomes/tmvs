module GUIsetupWidgets where

import Data.IORef
import Array
import Graphics.UI.Gtk
import Data.Array.IO

import IOArrayHelpers
import Types
import GUItypes
import GUIdrawing
import VirtualMachine

-- Init Widgets

initWindow fileName = do
    mainWindow <- windowNew
    set mainWindow [windowDefaultWidth := 1400, windowDefaultHeight := 800,
                    containerBorderWidth := 10]

    (fileNameLabel,fileNameFrame) <- labelWithFrameNew 
    frameSetLabel fileNameFrame "TM File"
    labelSetText fileNameLabel fileName

    mainHbox <- hBoxNew False 10
    containerAdd mainWindow mainHbox

    leftVbox <- vBoxNew False 10
    boxPackStart leftVbox fileNameFrame PackNatural 0

    rightPaned <- hPanedNew
    panedSetPosition rightPaned 330
 
    boxPackStart mainHbox leftVbox PackNatural 0
    boxPackStart mainHbox rightPaned PackGrow 0

    return (mainWindow,leftVbox,rightPaned )

initStackWidgets dmemMaxAddr = do
    stackScrWin <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy stackScrWin PolicyAutomatic PolicyAutomatic
  
    stackTable <- tableNew 5 dmemMaxAddr False

    stackEntryArray <- createNew2dEntryArray (dmemMaxAddr,3)
    setColumnProperties stackEntryArray 0 [entryWidthChars := 4, entryEditable := False]
    setColumnProperties stackEntryArray 1 [entryWidthChars := 4, entryEditable := True]
    setColumnProperties stackEntryArray 2 [entryWidthChars := 8, entryEditable := False]
    setColumnProperties stackEntryArray 3 [entryWidthChars := 16, entryEditable := False]

    attachEntriesToTable stackTable stackEntryArray 1 4
    stackArrowArray <- createAndAttachArrowsToTable stackTable dmemMaxAddr
    scrolledWindowAddWithViewport stackScrWin stackTable 

    stackLabel <- labelNew (Just "Data Memory")

    stackFollowCheckButton <- checkButtonNewWithLabel "Follow FP"
    stackEditableCheckButton <- checkButtonNewWithLabel "Editable"

    stackButtonHbox <- hBoxNew False 10
    boxPackStart stackButtonHbox stackFollowCheckButton PackNatural 0
    --boxPackStart stackButtonHbox stackEditableCheckButton PackNatural 0

    stackVbox <- vBoxNew False 10
    boxPackStart stackVbox stackLabel PackNatural 0
    boxPackStart stackVbox stackScrWin PackGrow 0
    boxPackStart stackVbox stackButtonHbox PackNatural 0

    return (stackVbox, 
            stackScrWin,
            stackTable,
            stackEntryArray, 
            stackArrowArray, 
            stackFollowCheckButton, 
            stackEditableCheckButton)

initInstWidgets stateListRef = do

    stateList <- readIORef stateListRef
    programArray <- return (getTMstateimem (head stateList)) --program :: Array Int Instruction
    programAddrEnd <- return (snd (bounds programArray))

    instScrWin <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy instScrWin PolicyAutomatic PolicyAutomatic

    instTable <- tableNew 7 programAddrEnd False

    instEntryArray <- createNew2dEntryArray (programAddrEnd,6)
    setColumnProperties instEntryArray 0 [entryWidthChars := 4, entryEditable := False]
    setColumnProperties instEntryArray 1 [entryWidthChars := 4, entryEditable := True]
    setColumnProperties instEntryArray 2 [entryWidthChars := 4, entryEditable := False]
    setColumnProperties instEntryArray 3 [entryWidthChars := 4, entryEditable := False]
    setColumnProperties instEntryArray 4 [entryWidthChars := 4, entryEditable := False]
    setColumnProperties instEntryArray 5 [entryWidthChars := 55, entryEditable := False]
    setInstEntryText instEntryArray programArray 0 programAddrEnd  


    attachEntriesToTable instTable instEntryArray 1 6
    instArrowArray <- createAndAttachArrowsToTable instTable programAddrEnd
    scrolledWindowAddWithViewport instScrWin instTable

    instLabel <- labelNew (Just "Instruction Memory")

    instFollowCheckButton <- checkButtonNewWithLabel "Follow PC"
   
    instButtonHbox <- hBoxNew False 10
    boxPackStart instButtonHbox instFollowCheckButton PackNatural 0
 
    instVbox <- vBoxNew False 10
    boxPackStart instVbox instLabel PackNatural 0
    boxPackStart instVbox instScrWin PackGrow 0
    boxPackStart instVbox instButtonHbox PackNatural 0

    return (instVbox, instScrWin, instTable, instEntryArray, instArrowArray, instFollowCheckButton)
    where setInstEntryText entries programArray i iEnd
            | i > iEnd = return ()
            | isROinstruction (programArray!i) = do
                (RO op r s t comment) <- return (programArray!i)
                addrEntry <- ix2d entries i 0
                entrySetText addrEntry (show i)    
                opEntry <- ix2d entries i 1
                entrySetText opEntry (show op)
                rEntry <- ix2d entries i 2
                entrySetText rEntry (show r)
                sEntry <- ix2d entries i 3
                entrySetText sEntry (show s)
                tEntry <- ix2d entries i 4
                entrySetText tEntry (show t)
                commentEntry <- ix2d entries i 5
                entrySetText commentEntry comment
                setInstEntryText entries programArray (i+1) iEnd
            | isRMinstruction (programArray!i) = do
                (RM op r d s comment) <- return (programArray!i)
                addrEntry <- ix2d entries i 0
                entrySetText addrEntry (show i)    
                opEntry <- ix2d entries i 1
                entrySetText opEntry (show op)
                rEntry <- ix2d entries i 2
                entrySetText rEntry (show r)
                dEntry <- ix2d entries i 3
                entrySetText dEntry (show d)
                sEntry <- ix2d entries i 4
                entrySetText sEntry (show s)
                commentEntry <- ix2d entries i 5
                entrySetText commentEntry comment
                setInstEntryText entries programArray (i+1) iEnd

createAndAttachArrowsToTable instTable programAddrEnd = do           
    arrowArray <- (newArray_ (0,programAddrEnd) :: IO (IOArray Int Arrow))
    createAndAttachArrowsToTableHelper instTable arrowArray programAddrEnd
    where
        createAndAttachArrowsToTableHelper instTable arrowArray i
            | i < 0 = return arrowArray
            | otherwise = do
                arrow <- arrowNew ArrowRight ShadowEtchedIn
                writeArray arrowArray i arrow
                tableAttach instTable arrow 0 1 i (i+1) [] [] 0 0
                createAndAttachArrowsToTableHelper instTable arrowArray (i-1)

initRegWidgets = do
    entryArray <- (newArray_ (0,7) :: IO (IOArray Int Entry))
    regTable <- tableNew 4 4 False
    createAndAttachRegEntries entryArray regTable 0
    createAndAttachLabel regTable "ac: " 0 0
    createAndAttachLabel regTable "ac1:" 1 0 
    createAndAttachLabel regTable "r2: " 2 0 
    createAndAttachLabel regTable "r3: " 3 0 
    createAndAttachLabel regTable "r4:" 0 2 
    createAndAttachLabel regTable "fp:" 1 2 
    createAndAttachLabel regTable "gp:" 2 2 
    createAndAttachLabel regTable "pc:" 3 2 

    regHbox <- hBoxNew False 0
    boxPackStart regHbox regTable PackRepel 0

    regFrame <- frameNew
    frameSetLabel regFrame "Registers"
    containerAdd regFrame regHbox
        
    return (regFrame,regTable,entryArray)

    where 
        createAndAttachRegEntries entryArray regTable i
            | i>7 = return()
            | otherwise = do
                entry <- entryNew
                set entry [entryWidthChars := 4, entryEditable := False]
                writeArray entryArray i entry
                tableAttach regTable entry col (col+1) row (row+1) [] [] 0 0         
                createAndAttachRegEntries entryArray regTable (i+1)
                where
                    row = i `mod` 4
                    col = if i<4 then 1 else 3
        createAndAttachLabel regTable str i j = do
            label <- labelNew Nothing
            labelSetText label str
            tableAttach regTable label j (j+1) i (i+1) [] [] 20 0



initControlWidgets = do
    stepBackXButton <- buttonNewWithLabel "backX"
    stepBackButton <- buttonNewWithLabel "back1"
    stepButton <- buttonNewWithLabel "step"
    stepXButton <- buttonNewWithLabel "stepX"
    stepEndButton <- buttonNewWithLabel "stepEnd"

    stepBackXEntry <- entryNew   
    set stepBackXEntry [entryWidthChars := 5, entryEditable := True]
    entrySetText stepBackXEntry "10"
    stepXEntry <- entryNew
    set stepXEntry [entryWidthChars := 5, entryEditable := True]
    entrySetText stepXEntry "10"

    controlButtonTable <- tableNew 2 5 True
    tableAttach controlButtonTable stepBackXButton 0 1 0 1 [] [] 0 0
    tableAttach controlButtonTable stepBackButton 1 2 0 2 [Fill] [Fill] 0 0
    tableAttach controlButtonTable stepButton 2 3 0 2 [Fill] [Fill] 0 0
    tableAttach controlButtonTable stepXButton 3 4 0 1 [] [] 0 0
    tableAttach controlButtonTable stepEndButton 4 5 0 2 [Fill] [Fill] 0 0
    tableAttach controlButtonTable stepBackXEntry 0 1 1 2 [] [] 0 0
    tableAttach controlButtonTable stepXEntry 3 4 1 2 [] [] 0 0
    
    currentStepTextLabel <- labelNew (Just "Step: ")
    currentStepLabel <- labelNew Nothing
    currentStepHbox <- hBoxNew False 0
    boxPackStart currentStepHbox currentStepTextLabel PackNatural 0
    boxPackStart currentStepHbox currentStepLabel PackNatural 0

    stepResultTextLabel <- labelNew (Just "Step Result: ")
    stepResultLabel <- labelNew Nothing
    stepResultHbox <- hBoxNew False 0
    boxPackStart stepResultHbox stepResultTextLabel PackNatural 0
    boxPackStart stepResultHbox stepResultLabel PackNatural 0    

    controlVbox <- vBoxNew False 10
    boxPackStart controlVbox controlButtonTable PackNatural 5
    boxPackStart controlVbox currentStepHbox PackNatural 0
    boxPackStart controlVbox stepResultHbox PackNatural 5

    controlFrame <- frameNew
    frameSetLabel controlFrame "Control"
    containerAdd controlFrame controlVbox

    return (controlFrame, 
            stepBackXButton, 
            stepBackButton, 
            stepButton, 
            stepXButton, 
            stepEndButton, 
            stepBackXEntry, 
            stepXEntry,
            currentStepLabel,
            stepResultLabel)

initOutputWidgets = do    
    outputFrame <- frameNew
    frameSetLabel outputFrame "IO Log"
    frameSetShadowType outputFrame ShadowOut

    outputScrWin <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy outputScrWin PolicyAutomatic PolicyAutomatic

    outputLabel <- labelNew Nothing
    labelSetSelectable outputLabel True
    labelSetJustify outputLabel JustifyLeft

    scrolledWindowAddWithViewport outputScrWin outputLabel
    containerAdd outputFrame outputScrWin

    return (outputFrame, outputScrWin, outputLabel)
    

-- Helpers

labelWithFrameNew :: IO (Label,Frame)
labelWithFrameNew = do
  label <- labelNew Nothing
  frame <- frameNew
  containerAdd frame label
  return (label, frame)

attachEntriesToTable table entries startAttachAtCol endAttachAtCol = do
    (iSize,_) <- dim2d entries
    attachEntriesToTableI table entries startAttachAtCol endAttachAtCol 0 iSize

attachEntriesToTableI table entries startAttachAtCol endAttachAtCol i iSize
    | i > iSize = return ()
    | otherwise = do
        (_,jSize) <- dim2d entries
        attachEntriesToTableJ table entries i iSize startAttachAtCol endAttachAtCol 0 jSize
        attachEntriesToTableI table entries startAttachAtCol endAttachAtCol (i+1) iSize
   
attachEntriesToTableJ table entries i iSize currentCol endCol j jSize
    | currentCol > endCol = return ()
    | otherwise = do
        entry <- ix2d entries i j
        tableAttach table entry currentCol (currentCol+1) i (i+1) [] [] 0 0
        attachEntriesToTableJ table entries i iSize (currentCol+1) endCol (j+1) jSize

