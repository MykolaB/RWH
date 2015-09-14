{-# LANGUAGE FlexibleInstances #-}
import JSONClass

instance (JSON a) => JSON [(String,a)] where
    toJValue = undefined
    fromJValue = undefined
