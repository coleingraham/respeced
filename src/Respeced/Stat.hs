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
,NaturalStats(..)
,StatModifier(..)
,CurrentStats(..)
,HasConsumableStats(..)
,CurrentConsumableStats(..)
,ConsumableStatModifier(..)
,applyStatModifiers
,baseStats
,emptyStats
,ConsumableStats(..)
-- *Alloccation
,Unused
,Total
,StatPoint(..)
,usedStatPoints
,calcTotalStatPoints
,calcUnusedStatPoints
)   where

import           Data.Monoid

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

data ConsumableStats = ConsumableStats {
         currentHP      :: !HP
        ,currentMP      :: !MP
        ,currentStamina :: !Stamina
    } deriving (Show,Eq,Ord)

class HasConsumableStats a where
    getHP      :: a -> HP
    getMP      :: a -> MP
    getStamina :: a -> Stamina

    setHP      :: a -> HP      -> a
    setMP      :: a -> MP      -> a
    setStamina :: a -> Stamina -> a

instance HasConsumableStats ConsumableStats where
    getHP      = currentHP
    getMP      = currentMP
    getStamina = currentStamina

    setHP      a x = a { currentHP      = x }
    setMP      a x = a { currentMP      = x }
    setStamina a x = a { currentStamina = x }

instance Monoid ConsumableStats where
    mempty = ConsumableStats 0 0 0
    mappend (ConsumableStats aHp aMp aSt)
            (ConsumableStats bHp bMp bSt)
           = ConsumableStats
                (aHp + bHp)
                (aMp + bMp)
                (aSt + bSt)

newtype CurrentConsumableStats = CurrentConsumableStats {
        unCurrentConsumableStats :: ConsumableStats
    } deriving (Show,Eq,Ord)

newtype ConsumableStatModifier = ConsumableStatModifier {
        unConsumableStatModifier :: ConsumableStats
    } deriving (Show,Eq,Ord)

-- |Represents a collection of stats for use with the combat system.
data Stats = Stats {
         consumableStats :: !ConsumableStats
        ,physicalOffense :: !(Offense Physical)
        ,physicalDefense :: !(Defense Physical)
        ,magicOffense    :: !(Offense Magic)
        ,magicDefense    :: !(Defense Magic)
        ,speed           :: !Speed
    } deriving (Show,Eq,Ord)

instance HasConsumableStats Stats where
    getHP      = getHP      . consumableStats
    getMP      = getMP      . consumableStats
    getStamina = getStamina . consumableStats

    setHP      a x = a { consumableStats = setHP      (consumableStats a) x }
    setMP      a x = a { consumableStats = setMP      (consumableStats a) x }
    setStamina a x = a { consumableStats = setStamina (consumableStats a) x }

instance Monoid Stats where
    mempty = emptyStats
    mappend (Stats aCS aPo aPd aMo aMd aSp)
            (Stats bCS bPo bPd bMo bMd bSp)
           = Stats
                (aCS <> bCS)
                (aPo + bPo)
                (aPd + bPd)
                (aMo + bMo)
                (aMd + bMd)
                (aSp + bSp)

newtype NaturalStats = NaturalStats {
        unNaturalStats :: Stats
    } deriving (Show,Eq,Ord)

newtype CurrentStats = CurrentStats {
        unCurrentStats :: Stats
    } deriving (Show,Eq,Ord)

newtype StatModifier = StatModifier {
        unStatModifier :: Stats
    } deriving (Show,Eq,Ord)

-- |Calculate 'CurrentStats' by applying a stack of 'StatModifier's to your
-- 'NaturalStats'.
applyStatModifiers :: NaturalStats -> [StatModifier] -> CurrentStats
applyStatModifiers (NaturalStats n)
    = CurrentStats . (n <>) . mconcat . map unStatModifier

-- |The minimum values for all stats.
baseStats :: Stats
baseStats = Stats (ConsumableStats 1 1 1) 1 1 1 1 1

-- |Useful as a basis for 'StatModifier's.
emptyStats :: Stats
emptyStats = Stats (ConsumableStats 0 0 0) 0 0 0 0 0

-- |This will take the apprioriate values from the provide 'CurrentStats'.
fromCurrentStats :: CurrentStats -> ConsumableStats
fromCurrentStats (CurrentStats s) = consumableStats s

-- |This will apply any stat changes to the provided 'ConsumableStats'. Use this
-- when you stats change in order to ensure that your 'ConsumableStats' are not
-- higher than they then should be.
applyCurrentStats
    :: CurrentStats
    -> CurrentConsumableStats
    -> CurrentConsumableStats
applyCurrentStats (CurrentStats s) (CurrentConsumableStats a)
    = CurrentConsumableStats
    $ a { currentHP      = min (getHP      a) (getHP      s)
        , currentMP      = min (getMP      a) (getMP      s)
        , currentStamina = min (getStamina a) (getStamina s) }

----

-- |Phantom type for unused 'StatPoint's.
data Unused

-- |Phantom type for total 'StatPoint's.
data Total

-- |The stat for stat points.
newtype StatPoint t = StatPoint {
        unStatPoint :: Int
    } deriving (Show,Eq,Ord,Enum,Bounded,Num)

-- |Calculate the total number of 'StatPoint's in use for the given 'Stats'.
usedStatPoints :: NaturalStats -> StatPoint Unused
usedStatPoints (NaturalStats
               (Stats
                (ConsumableStats
                      (HP      a)
                      (MP      b)
                      (Stamina c))

                      (Offense d)
                      (Defense e)
                      (Offense f)
                      (Defense g)
                      (Speed   h)))
    = StatPoint $ sum [a,b,c,d,e,f,g,h]

-- |Calculate the total number of 'StatPoint's required to support the provided
-- 'Stats' as well as the unused 'StatPoint's.
calcTotalStatPoints :: StatPoint Unused -> NaturalStats -> StatPoint Total
calcTotalStatPoints unused = StatPoint . unStatPoint . (unused +) . usedStatPoints

calcUnusedStatPoints :: StatPoint Total -> NaturalStats -> StatPoint Unused
calcUnusedStatPoints (StatPoint total)
    = StatPoint . (total -) . unStatPoint . usedStatPoints

