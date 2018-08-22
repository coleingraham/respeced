

module Respeced.Action (
 IsAction(..)
,Action(..)
)   where

import qualified Respeced.Stat  as S
import qualified Respeced.Time  as T

import           Data.Monoid

-- |Handles everything for dealing with action things.
class IsAction a where
    -- |Get the cost for using a function.
    getCost :: a -> S.ConsumableStats S.Modifier

    -- |Get whatever "damage" the action causes to the target.
    getConsumableEffect :: a -> S.ConsumableStats S.Modifier

    -- |Get the stat modifier, which can be 'mempty', the action causes to the
    -- target.
    getStatModifier :: a -> S.Stats S.Modifier

    -- |Apply the cost of the 'IsAction' instance to the provided stats. Note
    -- that the cost should be provided as positive numbers.
    handleCost
        :: a
        -> S.ConsumableStats S.Active
        -> S.ConsumableStats S.Active
    handleCost a = (f (getCost a) <>)
        where
            -- Change the phantom type and make each stat cost negative to work
            -- with the 'Sum' 'Monoid.
            f :: S.ConsumableStats S.Modifier -> S.ConsumableStats S.Active
            f (S.ConsumableStats h m s)
                = S.ConsumableStats
                  (negate h)
                  (negate m)
                  (negate s)

    -- |Apply the "damage" of the 'IsAction' instance to the provided stats.
    -- Note that actual damage must be provided as negative numbers, and healing
    -- as positive.
    handleConsumableEffect
        :: a
        -> S.ConsumableStats S.Active
        -> S.ConsumableStats S.Active
    handleConsumableEffect a = (f (getConsumableEffect a) <>)
        where
            f :: S.ConsumableStats S.Modifier -> S.ConsumableStats S.Active
            f (S.ConsumableStats h m s) = S.ConsumableStats h m s

-- |Represents something that an actor can do, how long it takes, and what the
-- results are.
data Action = Action {
        -- handled once on use
         actionCost                   :: !(S.ConsumableStats S.Modifier)
        ,actionTimeProperties         :: !T.TimeProperties
        -- handled for duration of action
        ,actionTargetConsumableEffect :: !(S.ConsumableStats S.Modifier)
        ,actionTargetStatEffect       :: !(S.Stats S.Modifier)
        ,actionTimeRemaining          :: !T.Tick
    } deriving (Show,Eq)

instance T.HasTimeProperties Action where
    getTimeProperties = actionTimeProperties

instance T.HasDuration Action where
    getRemainingTime        = actionTimeRemaining
    updateRemainingTime t a = a { actionTimeRemaining = t }

instance IsAction Action where
    getCost             = actionCost
    getConsumableEffect = actionTargetConsumableEffect
    getStatModifier     = actionTargetStatEffect
