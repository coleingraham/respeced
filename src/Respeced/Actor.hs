{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Respeced.Actor (
 unusedStatPoints
,calcTotalStatPoints
,StatPreset(..)
,StatPresets
,MaxPresets(..)
,Actor(..)
,currentStats
,addPreset
,ActorUpdate
,setActivePreset
,nextPreset
,prevPreset
,setStats
-- *Increasing stats
,increaseHP
,increaseMP
,increaseStamina
,increasePhysicalOffense
,increasePhysicalDefense
,increaseMagicOffense
,increaseMagicDefense
,increaseSpeed
-- *Decreasing stats
,decreaseHP
,decreaseMP
,decreaseStamina
,decreasePhysicalOffense
,decreasePhysicalDefense
,decreaseMagicOffense
,decreaseMagicDefense
,decreaseSpeed
,applyActorUpdate

,testActor
) where

import           Respeced.Stat
import           Respeced.Stat.Allocation

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid

-- |A name for a preset for an 'Actor's stats.
newtype StatPreset = StatPreset {
        unStatPreset :: Int
    } deriving (Show,Eq,Ord,Enum,Num,Hashable)

instance Bounded StatPreset where
    minBound = 1
    maxBound = 3

-- |A colletion of 'StatPreset's.
type StatPresets = HM.HashMap StatPreset (Stats Natural)

-- |The maximum number of 'StatPreset's allowed.
newtype MaxPresets = MaxPresets {
        unMaxPresets :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- ||Represents an actor.
data Actor = Actor {
         actorName        :: !String
        ,statPresets      :: !StatPresets
        ,activePreset     :: !StatPreset
        ,maxPresets       :: !MaxPresets
        ,totalStatPoints  :: !(StatPoint Total)
    } deriving (Show,Eq,Ord)

unusedStatPoints :: Actor -> StatPoint Unused
unusedStatPoints a = calcUnusedStatPoints (totalStatPoints a) c
    where
        c = currentStats a

-- |Get the currently active 'Stats' from an 'Actor'.
currentStats :: Actor -> Stats Natural
currentStats a = statPresets a HM.! activePreset a

-- |Add a new preset slot to an 'Actor'.
addPreset :: Actor -> Actor
addPreset a = a { maxPresets  = mx
                , statPresets = hm }
    where
        mx = succ $ maxPresets a
        hm = HM.insert (StatPreset $ unMaxPresets mx) baseStats $ statPresets a

-- |When updating an 'Actor', the result can be either the updated 'Actor' or an
-- error message.
type ActorUpdate = Either String Actor

setActivePreset :: StatPreset-> Actor -> ActorUpdate
setActivePreset k a
    | k `elem` HM.keys hm = Right $ a { activePreset = k }
    | otherwise           = Left $ show (unStatPreset k) <> " is not a valid preset."
    where
        hm = statPresets a

nextPreset :: Actor -> ActorUpdate
nextPreset a = setActivePreset p a
    where
        x = unStatPreset $ activePreset a
        y = unMaxPresets $ maxPresets a
        p = if x == y then 1 else StatPreset (succ x)

prevPreset :: Actor -> ActorUpdate
prevPreset a = setActivePreset p a
    where
        x = unStatPreset $ activePreset a
        y = unMaxPresets $ maxPresets a
        p = if x == 1 then StatPreset y else StatPreset (pred x)

setStats :: Stats Natural -> Actor -> Actor
setStats s a = a {  statPresets = HM.insert k s hm }
    where
        k  = activePreset a
        hm = statPresets a

alterStat
    :: Num a
    => (Stats Natural -> a)                  -- ^the "getter" function
    -> (Stats Natural -> a -> Stats Natural) -- ^the "setter" function
    -> (a -> a -> a)
    -> Actor
    -> Actor
alterStat getter setter op a = a { statPresets = stats' }
    where
        k      = activePreset a
        hm     = statPresets a
        stats  = hm HM.! k
        v      = setter stats (getter stats `op` 1)
        stats' = HM.insert k v hm

-- |If the 'Actor' has enough unused 'StatPoint's, allow them to trade one to
-- raise a stat by one.
increaseStat
    :: Num a
    => (Stats Natural -> a)                  -- ^the "getter" function
    -> (Stats Natural -> a -> Stats Natural) -- ^the "setter" function
    -> Actor                                 -- ^the 'Actor' to update
    -> ActorUpdate
increaseStat getter setter a
    | unusedStatPoints a > 0 = Right a'
    | otherwise              = Left "no unused stat points."
    where
        a' = alterStat getter setter (+) a

increaseHP :: Actor -> ActorUpdate
increaseHP = increaseStat getHP setHP

increaseMP :: Actor -> ActorUpdate
increaseMP = increaseStat getMP setMP

increaseStamina :: Actor -> ActorUpdate
increaseStamina = increaseStat getStamina setStamina

increasePhysicalOffense :: Actor -> ActorUpdate
increasePhysicalOffense
    = increaseStat physicalOffense (\s x -> s {physicalOffense = x})

increasePhysicalDefense :: Actor -> ActorUpdate
increasePhysicalDefense
    = increaseStat physicalDefense (\s x -> s {physicalDefense = x})

increaseMagicOffense :: Actor -> ActorUpdate
increaseMagicOffense
    = increaseStat magicOffense (\s x -> s {magicOffense = x})

increaseMagicDefense :: Actor -> ActorUpdate
increaseMagicDefense
    = increaseStat magicDefense (\s x -> s {magicDefense = x})

increaseSpeed :: Actor -> ActorUpdate
increaseSpeed = increaseStat speed (\s x -> s {speed = x})

-- |If the 'Actor' has a stat above one, allow them to trade a point in that stat
-- for an unused 'StatPoint'.
decreaseStat
    :: (Num a,Ord a)
    => (Stats Natural -> a)                  -- ^the "getter" function
    -> (Stats Natural -> a -> Stats Natural) -- ^the "setter" function
    -> String                                -- ^the name of the stat to be displayed for an error
    -> Actor                                 -- ^the 'Actor' to update
    -> ActorUpdate
decreaseStat getter setter nom a
    | getter stats > 1 = Right a' -- $ a { statPresets = stats' }
    | otherwise        = Left $ nom <> " is already the lowest possible value."
    where
        stats = statPresets a HM.! activePreset a
        a'    = alterStat getter setter (-) a

decreaseHP :: Actor -> ActorUpdate
decreaseHP = decreaseStat getHP setHP "hp"

decreaseMP :: Actor -> ActorUpdate
decreaseMP = decreaseStat getMP setMP "mp"

decreaseStamina :: Actor -> ActorUpdate
decreaseStamina = decreaseStat getStamina setStamina "stamina"

decreasePhysicalOffense :: Actor -> ActorUpdate
decreasePhysicalOffense
    = decreaseStat physicalOffense (\s x -> s {physicalOffense = x}) "physical offense"

decreasePhysicalDefense :: Actor -> ActorUpdate
decreasePhysicalDefense
    = decreaseStat physicalDefense (\s x -> s {physicalDefense = x}) "physical defense"

decreaseMagicOffense :: Actor -> ActorUpdate
decreaseMagicOffense
    = decreaseStat magicOffense (\s x -> s {magicOffense = x}) "magic offense"

decreaseMagicDefense :: Actor -> ActorUpdate
decreaseMagicDefense
    = decreaseStat magicDefense (\s x -> s {magicDefense = x}) "magic defense"

decreaseSpeed :: Actor -> ActorUpdate
decreaseSpeed = decreaseStat speed (\s x -> s {speed = x}) "speed"

applyActorUpdate :: (Actor -> ActorUpdate) -> Actor -> (Actor,String)
applyActorUpdate f a = case f a of
    Right x -> (x,"")
    Left  x -> (a,x)

----

testActor :: StatPoint Total -> Actor
testActor = Actor nom hm k mx
    where
        nom = "Test Actor"
        k   = 1
        s   = baseStats
        hm  = HM.fromList [(k,s),(2,s)]
        mx  = MaxPresets $ HM.size hm
