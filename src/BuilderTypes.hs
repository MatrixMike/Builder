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

data Item    = Item    (Name, Value) deriving (Show)
data Module  = Module  {mName::Name, items :: [Item], deps :: Deps}  deriving (Show)
data Build   = Build   [Module] deriving (Show)
data Project = Project {env :: Env, buil :: Build, deploy :: Deploy} deriving (Show)

-- Get an item by Name from a list of Item 
itemByName :: Name -> [Item] -> Either String Item
itemByName nm [] = Left ("No Item with name:" ++ (show nm))
itemByName nm (x:xs)
 | nm == n' = Right x
 | otherwise = itemByName nm xs 
 where Item (n', _) = x


itemByNameInModule :: Module -> Name  -> Either String Item
itemByNameInModule m n =itemByName n (items m) 

-- ------------------------------------------------------
moduleName :: Module -> Value
moduleName m =
    case itemByName "name" (items m) of
        Right (Item (_,  v)) -> v
        Left _ -> "Module has no name!!"::Value
-- ------------------------------------------------------
moduleByName :: Name -> Project -> Either String Module
moduleByName n p =
    case  filter (\m -> mName m == n ) mods  of
        []     -> Left ("No module with name: " ++ (show n))
        (a:[]) -> Right a
        (_:_)  -> Left ("Duplicate modeules with name: " ++ (show n)) -- won't happen!
        where 
            Build mods =  buil p
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

