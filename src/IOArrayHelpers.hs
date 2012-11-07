module IOArrayHelpers where

import Graphics.UI.Gtk
import Data.Array.IO


createNew2dEntryArray (i,j) = do
    arr <- (newArray_ (0,i) :: IO (IOArray Int (IOArray Int Entry)))
    createNew2dEntryArrayHelper arr i j
    return arr

createNew2dEntryArrayHelper _ (-1) _ = return ()
createNew2dEntryArrayHelper arr i j = do
    arrj <- newArray_ (0,j) :: IO (IOArray Int Entry)
    populateArrayWithEntries arrj j
    writeArray arr i arrj
    createNew2dEntryArrayHelper arr (i-1) j

populateArrayWithEntries arr (-1) = return ()
populateArrayWithEntries arr i = do
    entry <- entryNew
    writeArray arr i entry
    populateArrayWithEntries arr (i-1)

assign1d array x e = do
    writeArray array x e

ix1d array x = do 
    readArray array x

assign2d array x y e = do
    innerArray <- readArray array x
    writeArray innerArray y e

ix2d array x y = do
    --(a,b) <- getBounds innerArray
    --print (a,b)
    innerArray <- readArray array x
    readArray innerArray y

dim2d :: (IOArray Int (IOArray Int Entry)) -> IO (Int, Int)
dim2d array = do
    (_,i) <- getBounds array
    innerArray0 <- readArray array 0 --would be safer to read at i since we just checked it is in bounds
    (_,j) <- getBounds innerArray0
    return (i,j)
