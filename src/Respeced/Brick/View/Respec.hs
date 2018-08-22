{-# LANGUAGE OverloadedStrings #-}

module Respeced.Brick.View.Respec where

import           Respeced.Actor
import           Respeced.Stat
import           Respeced.Stat.Allocation
import           Respeced.Brick.State

import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core   as W
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
        p     = printf "%3d" . getSum
        a     = playerActor rs
        s     = currentStats a

        ui    = W.hBox [left,right]

        left  = W.vBox stng
        stng  = [ W.padTop (T.Pad 1)
                $ W.str $ "Unused Stat Points: " <> p (unStatPoint $ unusedStatPoints a)
                , W.str $ "    Current Preset: "
                  <> show (unStatPreset $ activePreset a)
                  <> "/"
                  <> show (unMaxPresets $ maxPresets a)]

        right = W.vBox [stats,notes]

        stats = B.borderWithLabel (W.str "Stats")
              $ W.vBox
              [ W.str   "                     (+/-)"
              , W.str $ "              HP:" <> p (unHP      $ getHP           s) <> " (q/Q)"
              , W.str $ "              MP:" <> p (unMP      $ getMP           s) <> " (w/W)"
              , W.str $ "         Stamina:" <> p (unStamina $ getStamina      s) <> " (e/E)"
              , W.str $ "Physical Offense:" <> p (unOffense $ physicalOffense s) <> " (r/R)"
              , W.padTop (T.Pad 1)
              $ W.str $ "Physical Defense:" <> p (unDefense $ physicalDefense s) <> " (a/A)"
              , W.str $ "  Macgic Offense:" <> p (unOffense $ magicOffense    s) <> " (s/S)"
              , W.str $ "  Macgic Defense:" <> p (unDefense $ magicDefense    s) <> " (d/D)"
              , W.str $ "           Speed:" <> p (unSpeed   $ speed           s) <> " (f/F)"]

        notes = W.vBox [W.str "z resets all stats to 1"
                       ,W.str ",/. change between presets"]

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

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar ',') []))
    = M.continue $ rs { playerActor = a }
    where
        a = fst $ applyActorUpdate prevPreset $ playerActor rs

handleEvent rs (T.VtyEvent (V.EvKey (V.KChar '.') []))
    = M.continue $ rs { playerActor = a }
    where
        a = fst $ applyActorUpdate nextPreset $ playerActor rs

handleEvent rs _ = M.continue rs
