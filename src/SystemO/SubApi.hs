{-# LANGUAGE FlexibleInstances #-}

module SystemO.SubApi (isSubApiOf) where
import Data.OpenApi
import Control.Lens
import Data.OpenApi.Lens
import Data.Function
import Data.HashMap.Strict.InsOrd

class SubApi a where
    subApiOf :: a -> a -> Bool

instance SubApi OpenApi where
  subApiOf a b | a == b = True
  subApiOf a b = paths' a b
    where 
        paths' = on subApiOf $ view paths

instance SubApi (InsOrdHashMap FilePath PathItem) where
  subApiOf a b | size a == 0 = True
  subApiOf a b = False

isSubApiOf :: OpenApi  -> OpenApi -> Bool
isSubApiOf  = subApiOf