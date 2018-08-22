{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Respeced.Stat.Allocation (
 Unused
,Total
,StatPoint(..)
,usedStatPoints
,calcTotalStatPoints
,calcUnusedStatPoints
)   where

import           Respeced.Stat

import           Data.Monoid

-- |Phantom type for unused 'StatPoint's.
data Unused

-- |Phantom type for total 'StatPoint's.
data Total

-- |The stat for stat points.
newtype StatPoint t = StatPoint {
        unStatPoint :: Sum Int
    } deriving (Show,Eq,Ord,Monoid,Num)

-- |Calculate the total number of 'StatPoint's in use for the given 'Stats'.
usedStatPoints :: Stats Natural -> StatPoint Unused
usedStatPoints (Stats
                (ConsumableStats
                      (HP      a)
                      (MP      b)
                      (Stamina c))

                      (Offense d)
                      (Defense e)
                      (Offense f)
                      (Defense g)
                      (Speed   h))
    = StatPoint $ mconcat [a,b,c,d,e,f,g,h]

-- |Calculate the total number of 'StatPoint's required to support the provided
-- 'Stats' as well as the unused 'StatPoint's.
calcTotalStatPoints :: StatPoint Unused -> Stats Natural -> StatPoint Total
calcTotalStatPoints unused = StatPoint . unStatPoint . (unused +) . usedStatPoints

calcUnusedStatPoints :: StatPoint Total -> Stats Natural -> StatPoint Unused
calcUnusedStatPoints (StatPoint total)
    = StatPoint . (total -) . unStatPoint . usedStatPoints

