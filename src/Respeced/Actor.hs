{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Respeced.Actor (
 Physical
,Magic
,HP(..)
,MP(..)
,Stamina(..)
,Offense(..)
,Defense(..)
,Speed(..)
,Stats(..)
,baseStats
,Unused
,Total
,StatPoint(..)
,usedStatPoints
,unusedStatPoints
,calcTotalStatPoints
,StatPreset(..)
,StatPresets
,MaxPresets(..)
,Actor(..)
,currentStats
,addPreset
,deletePreset
,ActorUpdate
,setActivePreset
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

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import           Data.String         (IsString)

-- Phantom type for physical things.
data Physical

-- Phantom type for magic things.
data Magic

-- |The stat for health points.
newtype HP = HP {
        unHP :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |The stat for magic points.
newtype MP = MP {
        unMP :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |The stat for stamina.
newtype Stamina = Stamina {
        unStamina :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |The stat for damage of various types.
newtype Offense t = Offense {
        unOffense :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |The stat for defense of various types.
newtype Defense t = Defense {
        unDefense :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |The stat for speed.
newtype Speed = Speed {
        unSpeed :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |Represents a collection of stats for use with the combat system.
data Stats = Stats {
         hp              :: !HP
        ,mp              :: !MP
        ,stamina         :: !Stamina
        ,physicalOffense :: !(Offense Physical)
        ,physicalDefense :: !(Defense Physical)
        ,magicOffense    :: !(Offense Magic)
        ,magicDefense    :: !(Defense Magic)
        ,speed           :: !Speed
    } deriving (Show,Eq,Ord)

-- |The minimum values for all stats.
baseStats :: Stats
baseStats = Stats 1 1 1 1 1 1 1 1

-- |Phantom type for unused 'StatPoint's.
data Unused

-- |Phantom type for total 'StatPoint's.
data Total

-- |The stat for stat points.
newtype StatPoint t = StatPoint {
        unStatPoint :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |Calculate the total number of 'StatPoint's in use for the given 'Stats'.
usedStatPoints :: Stats -> StatPoint Unused
usedStatPoints (Stats (HP      a)
                      (MP      b)
                      (Stamina c)
                      (Offense d)
                      (Defense e)
                      (Offense f)
                      (Defense g)
                      (Speed   h))
    = StatPoint $ sum [a,b,c,d,e,f,g,h]

-- |Calculate the total number of 'StatPoint's required to support the provided
-- 'Stats' as well as the unused 'StatPoint's.
calcTotalStatPoints :: StatPoint Unused -> Stats -> StatPoint Total
calcTotalStatPoints unused = StatPoint . unStatPoint . (unused +) . usedStatPoints

calcUnusedStatPoints :: StatPoint Total -> Stats -> StatPoint Unused
calcUnusedStatPoints (StatPoint total)
    = StatPoint . (total -) . unStatPoint . usedStatPoints

-- |A name for a preset for an 'Actor's stats.
newtype StatPreset = StatPreset {
        unStatPreset :: String
    } deriving (Show,Eq,Ord,IsString,Hashable)

-- |A colletion of 'StatPreset's.
type StatPresets = HM.HashMap StatPreset Stats

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
currentStats :: Actor -> Stats
currentStats a = statPresets a HM.! activePreset a

-- |Add or overwrite the provided 'StatPreset' for the given 'Actor'.
addPreset :: StatPreset -> Stats -> Actor -> Actor
addPreset k v a = a { statPresets = HM.insert k v (statPresets a) }

deletePreset :: StatPreset -> Actor -> Actor
deletePreset k a = a { statPresets = HM.delete k (statPresets a) }

-- |When updating an 'Actor', the result can be either the updated 'Actor' or an
-- error message.
type ActorUpdate = Either String Actor

setActivePreset :: StatPreset-> Actor -> ActorUpdate
setActivePreset k a
    | k `elem` HM.keys hm = Right $ a { activePreset = k }
    | otherwise           = Left $ unStatPreset k <> " is not a valid preset."
    where
        hm = statPresets a

setStats :: Stats -> Actor -> Actor
setStats s a = a {  statPresets = HM.insert k s hm }
    where
        k  = activePreset a
        hm = statPresets a

-- |If the 'Actor' has enough unused 'StatPoint's, allow them to trade one to
-- raise a stat by one.
increaseStat
    :: Num a
    => (Stats -> a)          -- ^the "getter" function
    -> (Stats -> a -> Stats) -- ^the "setter" function
    -> Actor                 -- ^the 'Actor' to update
    -> ActorUpdate
increaseStat getter setter a
    | unusedStatPoints a > 0 = Right $ a {  statPresets = stats' }
    | otherwise              = Left "no unused stat points."
    where
        k      = activePreset a
        hm     = statPresets a
        stats  = hm HM.! k
        stats' = HM.insert k (setter stats (getter stats + 1)) hm

increaseHP :: Actor -> ActorUpdate
increaseHP = increaseStat hp (\s x -> s {hp = x})

increaseMP :: Actor -> ActorUpdate
increaseMP = increaseStat mp (\s x -> s {mp = x})

increaseStamina :: Actor -> ActorUpdate
increaseStamina = increaseStat stamina (\s x -> s {stamina = x})

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
    => (Stats -> a)          -- ^the "getter" function
    -> (Stats -> a -> Stats) -- ^the "setter" function
    -> String                -- ^the name of the stat to be displayed for an error
    -> Actor                 -- ^the 'Actor' to update
    -> ActorUpdate
decreaseStat getter setter nom a
    | v > 1     = Right $ a { statPresets = stats' }
    | otherwise = Left $ nom <> " is already the lowest possible value."
    where
        k      = activePreset a
        v      = getter stats
        hm     = statPresets a
        stats  = hm HM.! k
        stats' = HM.insert k (setter stats (v - 1)) hm

decreaseHP :: Actor -> ActorUpdate
decreaseHP = decreaseStat hp (\s x -> s {hp = x}) "hp"

decreaseMP :: Actor -> ActorUpdate
decreaseMP = decreaseStat mp (\s x -> s {mp = x}) "mp"

decreaseStamina :: Actor -> ActorUpdate
decreaseStamina = decreaseStat stamina (\s x -> s {stamina = x}) "stamina"

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
testActor = Actor nom hm k 1
    where
        nom = "Test Actor"
        k   = "default"
        s   = baseStats
        hm  = HM.fromList [(k,s),("two",s)]
