{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Respeced.Stat(
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
,calcTotalStatPoints
,calcUnusedStatPoints
)   where

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

