module Main where

import           Respeced.Action()
import           Respeced.Actor
import           Respeced.Brick.State
import           Respeced.Brick.View.Respec (respecScreen)
import           Respeced.Time()

import qualified Brick.Main          as M
import           Control.Monad

main :: IO ()
main = testRun

testRun :: IO ()
testRun = void $ run initialState

run :: RespecState -> IO RespecState
run rs = go $ screen rs
    where
        go :: Screen -> IO RespecState
        go Exit   = return rs

        go Respec = M.defaultMain respecScreen rs

initialState :: RespecState
initialState = RespecState Respec vNone $ testActor 18
