{-# LANGUAGE OverloadedStrings #-}

module Respeced.Brick.State where

import           Respeced.Actor

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Data.Text            as T
import qualified Graphics.Vty         as V

-- |This is how brick keeps track of where the cursor focus is.
type View = T.Text

vTextEdit :: View
vTextEdit = "text_edit"

--vListView :: View
--vListView = "list_view"

vNone :: View
vNone = "none"

-- |Which screen we are on.
data Screen
    = Exit
    | Home
    | Respec
    | Combat [Actor] -- ^which 'Actor's you are fighting
    deriving (Show,Eq)

-- |The state used for all brick things.
data RespecState = RespecState {
         screen      :: !Screen
        ,view        :: !View
        ,playerActor :: !Actor
    } deriving (Show,Eq)

defAttrMap :: A.AttrMap
defAttrMap = A.attrMap V.defAttr []

defCursor :: RespecState -> [T.CursorLocation View] -> Maybe (T.CursorLocation View)
defCursor _ = M.showFirstCursor vNone

setScreen :: Screen -> RespecState -> RespecState
setScreen s rs = rs { screen = s }

setView :: View -> RespecState -> RespecState
setView v rs = rs { view = v }
