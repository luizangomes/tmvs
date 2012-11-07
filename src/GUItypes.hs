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


