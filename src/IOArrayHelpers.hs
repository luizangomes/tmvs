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
--<http://www.gnu.org/licenses/>.

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
