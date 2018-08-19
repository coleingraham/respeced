

module Respeced.Action where

import qualified Respeced.Stat as S
import           Respeced.Time

-- |Represents something that an actor can do, how long it takes, and what the
-- results are.
data Action = Action {
         actionCost                   :: !S.ConsumableStatModifier
        ,actionTimeProperties         :: !TimeProperties
        ,actionTargetConsumableEffect :: !S.ConsumableStatModifier
        ,actionTargetStatEffect       :: !S.StatModifier
    } deriving (Show,Eq)

instance HasTimeProperties Action where
    getTimeProperties = actionTimeProperties

