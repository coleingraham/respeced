{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Respeced.Time where

-- |A single unit of time.
newtype Tick = Tick {
        unTick :: Int
    } deriving (Show,Eq,Ord,Num)

-- |Represents the time an action takes in various contexts.
newtype Time a = Time {
        unTime :: Tick
    } deriving (Show,Eq,Ord,Num)

-- |For the 'Time' it takes to activate an action.
data Activation

-- |For the 'Time' it takes for an action to be usable again.
data Cooldown

-- |For the 'Time' it takes for the 'Actor' to be able to perform an action.
data Recovery

data TimeProperties = TimeProperties {
         activationTime :: !(Time Activation)
        ,revoceryTime   :: !(Time Recovery)
        ,cooldownTime   :: !(Time Cooldown)
    } deriving (Show,Eq)

class HasTimeProperties a where
    getTimeProperties :: a -> TimeProperties

instance HasTimeProperties TimeProperties where
    getTimeProperties = id

totalTime :: TimeProperties -> Tick
totalTime p = unTime (activationTime p)
            + unTime (revoceryTime   p)
            + unTime (cooldownTime   p)

data HasTimeProperties a => TimedEvent a = TimedEvent {
         event        :: !a
        ,ellapsedTime :: !Tick
    } deriving (Show,Eq)

instance (HasTimeProperties a) => HasTimeProperties (TimedEvent a) where
    getTimeProperties = getTimeProperties . event

startTimedEvent :: HasTimeProperties a => a -> TimedEvent a
startTimedEvent a = TimedEvent a 0

stepTimedEvent :: HasTimeProperties a => TimedEvent a -> TimedEvent a
stepTimedEvent x = x { ellapsedTime = ellapsedTime x + 1 }

timeUntilActivation :: HasTimeProperties a => TimedEvent a -> Tick
timeUntilActivation x = a - ellapsedTime x
    where
        a = unTime (activationTime $ getTimeProperties x)

timeUntilRecovered :: HasTimeProperties a => TimedEvent a -> Tick
timeUntilRecovered x = a + r - ellapsedTime x
    where
        p = getTimeProperties x
        a = unTime (activationTime p)
        r = unTime (revoceryTime   p)

timeUntilUsable :: HasTimeProperties a => TimedEvent a -> Tick
timeUntilUsable x = totalTime (getTimeProperties x) - ellapsedTime x

isActivated :: (HasTimeProperties a) => TimedEvent a -> Bool
isActivated = (<= 0) . timeUntilActivation

isRecovered :: (HasTimeProperties a) => TimedEvent a -> Bool
isRecovered = (<= 0) . timeUntilRecovered

isUsable :: (HasTimeProperties a) => TimedEvent a -> Bool
isUsable = (<= 0) . timeUntilUsable

----

testTimeProperties :: TimeProperties
testTimeProperties = TimeProperties 1 1 1

testTimedExvent :: TimedEvent TimeProperties
testTimedExvent = TimedEvent testTimeProperties 0
