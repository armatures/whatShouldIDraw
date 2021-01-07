{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module AdventConfig (adventConfig) where

import           Import
import           Text.XML.Cursor
adventConfig :: FilePath -> Int -> Config
adventConfig cwd i=
    Config
    { pathRaw = adventFilePath cwd i
    , pathClean = adventFilePathClean cwd i
    , parseContent = findNodes
    , url = adventUrl 1
    }


findNodes :: Cursor -> [Cursor]
findNodes = element "main" &/ element "article"

adventFilePath :: FilePath -> Int -> FilePath
adventFilePath cwd i
  = cwd ++ "/advents/" <> show i <>".html"

adventFilePathClean :: FilePath -> Int -> FilePath
adventFilePathClean cwd i
  = cwd ++ "/adventsClean/" <> show i <>".html"

adventUrl :: Int -> String
adventUrl i =
  "https://adventofcode.com/2020/day/" <> show i
