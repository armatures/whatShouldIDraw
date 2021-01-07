{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ArticleConfig (articleConfig) where

import           Import
import           RIO.List                   as List
import           Text.XML.Cursor

articleConfig :: FilePath -> Int -> Config
articleConfig cwd i=
    Config
    { pathRaw = articleFilePath cwd i
    , pathClean = articleFilePathClean cwd i
    , parseContent = parseArticle
    , url = articleUrl i
    }

parseArticle :: Cursor -> [Cursor]
parseArticle = element "main" &// element "article"

articleUrl i=
  let
    asString = show i
    paddedDigit = case List.length asString of
      1 -> "0"<> asString
      _ -> asString
  in
    "https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day"<> paddedDigit <>".md"

articleFilePath cwd i =
  cwd <> "/articles/" <> show i <> ".html"

articleFilePathClean cwd i =
  cwd <> "/articlesClean/" <> show i <> ".html"
