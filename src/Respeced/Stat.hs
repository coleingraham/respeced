{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Respeced.Stat (
-- *Individual Stats
 Physical
,Magic
,HP(..)
,MP(..)
,Stamina(..)
,Offense(..)
,Defense(..)
,Speed(..)
-- *Composit Stats
,Natural
,Current
,Modifier
,Active
,applyStatModifiers
,baseStats
,ConsumableStats(..)
,Stats(..)
,applyCurrentStats
,HasConsumableStats(..)
)   where

import           Data.Monoid

-- Phantom type for physical things.
data Physical

-- Phantom type for magic things.
data Magic

-- |The stat for health points.
newtype HP = HP {
        unHP :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |The stat for magic points.
newtype MP = MP {
        unMP :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |The stat for stamina.
newtype Stamina = Stamina {
        unStamina :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |The stat for damage of various types.
newtype Offense t = Offense {
        unOffense :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |The stat for defense of various types.
newtype Defense t = Defense {
        unDefense :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |The stat for speed.
newtype Speed = Speed {
        unSpeed :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

data Natural
data Current
data Modifier
data Active

data ConsumableStats a = ConsumableStats {
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

instance HasConsumableStats (ConsumableStats a) where
    getHP      = currentHP
    getMP      = currentMP
    getStamina = currentStamina

    setHP      a x = a { currentHP      = x }
    setMP      a x = a { currentMP      = x }
    setStamina a x = a { currentStamina = x }

instance Monoid (ConsumableStats a) where
    mempty = ConsumableStats mempty mempty mempty
    mappend (ConsumableStats aHp aMp aSt)
            (ConsumableStats bHp bMp bSt)
           = ConsumableStats
                (aHp <> bHp)
                (aMp <> bMp)
                (aSt <> bSt)

-- |Represents a collection of stats for use with the combat system.
data Stats a = Stats {
         consumableStats :: !(ConsumableStats a)
        ,physicalOffense :: !(Offense Physical)
        ,physicalDefense :: !(Defense Physical)
        ,magicOffense    :: !(Offense Magic)
        ,magicDefense    :: !(Defense Magic)
        ,speed           :: !Speed
    } deriving (Show,Eq,Ord)

instance HasConsumableStats (Stats a) where
    getHP      = getHP      . consumableStats
    getMP      = getMP      . consumableStats
    getStamina = getStamina . consumableStats

    setHP      a x = a { consumableStats = setHP      (consumableStats a) x }
    setMP      a x = a { consumableStats = setMP      (consumableStats a) x }
    setStamina a x = a { consumableStats = setStamina (consumableStats a) x }

instance Monoid (Stats a) where
    mempty = Stats mempty mempty mempty mempty mempty mempty
    mappend (Stats aCS aPo aPd aMo aMd aSp)
            (Stats bCS bPo bPd bMo bMd bSp)
           = Stats
                (aCS <> bCS)
                (aPo <> bPo)
                (aPd <> bPd)
                (aMo <> bMo)
                (aMd <> bMd)
                (aSp <> bSp)

-- |Calculate 'CurrentStats' by applying a stack of 'StatModifier's to your
-- 'NaturalStats'.
applyStatModifiers :: Stats Natural -> [Stats Modifier] -> Stats Current
applyStatModifiers n
    = g . (f n <>) . mconcat
    where
        f :: Stats Natural -> Stats Modifier
        f (Stats (ConsumableStats h m s) po pd mo md sp)
            = Stats (ConsumableStats h m s) po pd mo md sp

        g :: Stats Modifier -> Stats Current
        g (Stats (ConsumableStats h m s) po pd mo md sp)
            = Stats (ConsumableStats h m s) po pd mo md sp

-- |The minimum values for all stats.
baseStats :: Stats Natural
baseStats = Stats (ConsumableStats 1 1 1) 1 1 1 1 1

-- |This will apply any stat changes to the provided 'ConsumableStats'. Use this
-- when you stats change in order to ensure that your 'ConsumableStats' are not
-- higher than they then should be.
applyCurrentStats
    :: Stats Current
    -> ConsumableStats Active
    -> ConsumableStats Active
applyCurrentStats s a
    = a { currentHP      = min (getHP      a) (getHP      s)
        , currentMP      = min (getMP      a) (getMP      s)
        , currentStamina = min (getStamina a) (getStamina s) }

