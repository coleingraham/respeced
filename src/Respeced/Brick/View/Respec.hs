{-# LANGUAGE OverloadedStrings #-}

module Respeced.Brick.View.Respec where

import           Respeced.Actor
import           Respeced.Brick.State

import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core   as W
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
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
              [W.str "Respec Character Here"
              ,actorStatsUI rs]

actorStatsUI :: RespecState -> T.Widget View
actorStatsUI rs = B.borderWithLabel (W.str $ actorName a)
                $ W.hBox [W.vBox lst,stats]
    where
        p :: Int -> String
        p   = printf "%3d"

        a   = playerActor rs
        s   = currentStats a
        lst = [W.str $ "Unused Stat Points: " <> show (unStatPoint $ unusedStatPoints a)
              ,W.str $ "Current Preset: " <> unStatPreset (activePreset a)
              ,W.str "(the letter increases the stat)"
              ,W.str "(shift+letter decreases the stat)"
              ,W.str "(z resets all stats to 1)"]
        stats = B.borderWithLabel (W.str "Stats")
              $ W.vBox
              [W.str $ "              HP (q) " <> p (unHP      $ hp              s)
              ,W.str $ "              MP (w) " <> p (unMP      $ mp              s)
              ,W.str $ "         Stamina (e) " <> p (unStamina $ stamina         s)
              ,W.str $ "Physical Offense (r) " <> p (unOffense $ physicalOffense s)
              ,W.str $ "Physical Defense (a) " <> p (unDefense $ physicalDefense s)
              ,W.str $ "  Macgic Offense (s) " <> p (unOffense $ magicOffense    s)
              ,W.str $ "  Macgic Defense (d) " <> p (unDefense $ magicDefense    s)
              ,W.str $ "           Speed (f) " <> p (unSpeed   $ speed           s)]

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

handleEvent rs _ = M.continue rs
