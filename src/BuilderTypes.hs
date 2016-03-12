{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BuilderTypes where


type Env    = String
type Deps   = [LibRef]
type Name   = String
type Value  = String
type Deploy = String

data LibRef  = LibRef  {grp :: String, artifact :: String, version :: String} deriving (Show)
data Build   = Build   [Module] deriving (Show)
data Item    = Item    (Name, Value) deriving (Show)
data Module  = Module  {items :: [Item], deps :: Deps}  deriving (Show)
data Project = Project {env :: Env, buil :: Build, deploy :: Deploy} deriving (Show)

-- Get an item by Name from a list of Item 
itemByName :: Name -> [Item] -> Maybe Item
itemByName _ [] = Nothing
itemByName nm (x:xs)
 | nm == n = Just x
 | otherwise = itemByName nm xs 
 where Item (n, _) = x
-- ------------------------------------------------------
itemNames :: Module -> [Name]
itemNames m = [ n | Item (n, _) <- items m]

-- Is the supplied name the name of a module
isModule :: Name -> Project -> Bool
isModule name proj =  name `elem` (moduleNames proj)

-- Get a list of all module names in the project
moduleNames :: Project -> [Name]
moduleNames p = res where
    Build mods =  buil p
    itms = concat [ items m | m <- mods ]
    x = filter  f itms where  f (Item (name, _)) = name == "name"
    res = [ v | Item (_, v) <- x]

