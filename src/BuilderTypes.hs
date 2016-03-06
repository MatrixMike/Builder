{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BuilderTypes where



type Env = String
type Deps = [LibRef]
type Name = String
type Value = String
type Deploy = String

data LibRef = LibRef {grp :: String, artifact :: String, version :: String} deriving (Show)
data Build = Build [Module] deriving (Show)
data Item = Item (Name, Value) deriving (Show)
data Module = Module {items :: [Item], deps :: Deps}  deriving (Show)
data Project = Project {env :: Env, build :: Build, deploy :: Deploy} deriving (Show)

itemByName :: Name -> [Item] -> Maybe Item
itemByName _ [] = Nothing
itemByName nm (x:xs)
 | nm == n = Just x
 | otherwise = itemByName nm xs 
 where Item (n, _) = x