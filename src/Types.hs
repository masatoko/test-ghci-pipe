{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Binary
import GHC.Generics (Generic)

data Foo = Foo { unFoo :: Int } deriving (Show, Read, Generic)

instance Binary Foo
