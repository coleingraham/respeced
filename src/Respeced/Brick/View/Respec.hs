{-# LANGUAGE OverloadedStrings #-}

module Respeced.Brick.View.Respec where

import           Respeced.Actor
import           Respeced.Stat
import           Respeced.Brick.State

import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core   as W
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import qualified Data.HashMap.Strict  as HM
import           Data.List
import           Data.Monoid
import qualified Graphics.Vty         as V
import           Text.Printf          (printf)

respecScreen :: M.App RespecState V.Event View
respecScreen = M.App {
         M.appDraw         = drawUI
        ,M.appChooseCursor = defCursor
        ,M.appHandleEvent  = handleEvent
        ,M.appAttrMap      = const defAttrMap
        ,M.appStartEvent   = return
    }

drawUI :: RespecState -> [T.Widget View]
drawUI rs = [ui]
    where
        ui  = B.borderWithLabel lbl
            $ W.vBox [ttl]
        lbl = W.str "Respec"
        ttl = C.center $ W.vBox
              [W.str $ actorName $ playerActor rs
              ,actorStatsUI rs]

actorStatsUI :: RespecState -> T.Widget View
actorStatsUI rs = ui
    where
        p     = printf "%3d"
        a     = playerActor rs
        s     = currentStats a

        ui    = W.hBox [left,right]

        left  = W.vBox stng
        stng  = [W.str $ "Unused Stat Points: " <> show (unStatPoint $ unusedStatPoints a)
                ,W.str $ "Current Preset: "     <> unStatPreset (activePreset a)]

        right = W.vBox [stats,notes]

        stats = B.borderWithLabel (W.str "Stats")
              $ W.vBox
              [W.str   "                     (+/-)"
              ,W.str $ "              HP:" <> p (unHP      $ hp              s) <> " (q/Q)"
              ,W.str $ "              MP:" <> p (unMP      $ mp              s) <> " (w/W)"
              ,W.str $ "         Stamina:" <> p (unStamina $ stamina         s) <> " (e/E)"
              ,W.str $ "Physical Offense:" <> p (unOffense $ physicalOffense s) <> " (r/R)"
              ,W.padTop (T.Pad 1)
              $W.str $ "Physical Defense:" <> p (unDefense $ physicalDefense s) <> " (a/A)"
              ,W.str $ "  Macgic Offense:" <> p (unOffense $ magicOffense    s) <> " (s/S)"
              ,W.str $ "  Macgic Defense:" <> p (unDefense $ magicDefense    s) <> " (d/D)"
              ,W.str $ "           Speed:" <> p (unSpeed   $ speed           s) <> " (f/F)"]

        notes = W.vBox [W.str "(z resets all stats to 1)"]

updateStat
    :: (Actor -> ActorUpdate)
    -> RespecState
    -> RespecState
updateStat f rs = rs { playerActor = a }
    where
        --  TODO: actually deal with the error message in the state here
        a = fst $ applyActorUpdate f $ playerActor rs

handleEvent
    :: RespecState
    -> T.BrickEvent View V.Event
    -> T.EventM View (T.Next RespecState)
handleEvent rs (T.VtyEvent (V.EvKey V.KEsc []))
    = M.halt (setScreen Exit rs)

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar '>') []))
    = M.continue rs
    where
        a  = playerActor rs
        p  = activePreset a
        ps = sort $ delete p $ HM.keys $ statPresets a

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'q') []))
    = M.continue $ updateStat increaseHP rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'Q') []))
    = M.continue $ updateStat decreaseHP rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'w') []))
    = M.continue $ updateStat increaseMP rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'W') []))
    = M.continue $ updateStat decreaseMP rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'e') []))
    = M.continue $ updateStat increaseStamina rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'E') []))
    = M.continue $ updateStat decreaseStamina rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'r') []))
    = M.continue $ updateStat increasePhysicalOffense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'R') []))
    = M.continue $ updateStat decreasePhysicalOffense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'a') []))
    = M.continue $ updateStat increasePhysicalDefense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'A') []))
    = M.continue $ updateStat decreasePhysicalDefense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 's') []))
    = M.continue $ updateStat increaseMagicOffense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'S') []))
    = M.continue $ updateStat decreaseMagicOffense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'd') []))
    = M.continue $ updateStat increaseMagicDefense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'D') []))
    = M.continue $ updateStat decreaseMagicDefense rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'f') []))
    = M.continue $ updateStat increaseSpeed rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'F') []))
    = M.continue $ updateStat decreaseSpeed rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar 'z') []))
    = M.continue $ rs { playerActor = a }
    where
        a = setStats baseStats $ playerActor rs

handleEvent rs _ = M.continue rs
