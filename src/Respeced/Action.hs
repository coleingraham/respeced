

module Respeced.Action where

import qualified Respeced.Actor as A
import           Respeced.Time

data Action = Action {
         thing          :: !Int
        ,timeProperties :: !TimeProperties
    } deriving (Show,Eq)

instance HasTimeProperties Action where
    getTimeProperties = timeProperties

