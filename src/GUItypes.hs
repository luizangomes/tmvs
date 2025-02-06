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

module GUItypes where

import Data.Array.IO
import Graphics.UI.Gtk
import Data.IORef

import Types
import VirtualMachine

type IOEntryArray = (IOArray Int Entry)
type IOEntry2dArray = (IOArray Int (IOArray Int Entry))

data Sim = Sim (IORef [TMstate]) (IORef [LogEntry]) GUI

type LogEntry = (Int, String)

data GUI = GUI {
    getStackWidgets :: StackWidgets,
    getInstWidgets :: InstWidgets,
    getRegWidgets :: RegWidgets,
    getControlWidgets :: ControlWidgets,
    getOutputWidgets :: OutputWidgets }

data StackWidgets = StackWidgets {
    getStackScrWin :: ScrolledWindow,
    getStackTable :: Table,
    getStackEntries :: IOEntry2dArray,
    getStackArrows :: (IOArray Int Arrow),
    getStackFollowCheckButton :: CheckButton,
    getStackEditableCheckButton :: CheckButton }

data InstWidgets = InstWidgets {
    getInstScrWin :: ScrolledWindow,
    getInstTable :: Table,
    getInstEntries :: IOEntry2dArray,
    getInstArrows :: (IOArray Int Arrow),
    getInstFollowCheckButton :: CheckButton }

data RegWidgets = RegWidgets {
    getRegTable :: Table,
    getRegEntries :: IOEntryArray }

data ControlWidgets = ControlWidgets {
    getStepBackXButton :: Button,
    getStepBackButton :: Button,
    getStepButton :: Button,
    getStepXButton :: Button,
    getStepEndButton :: Button,
    getStepBackXEntry :: Entry,
    getStepXEntry :: Entry,
    getCurrentStepLabel :: Label,
    getStepResultLabel :: Label }

data OutputWidgets = OutputWidgets {
    getOutputScrWin :: ScrolledWindow,
    getOutputLabel :: Label }


