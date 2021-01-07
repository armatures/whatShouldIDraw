{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import           Data.ByteString.Lazy.Char8 as C hiding (ByteString)
import           Import
import           Network.HTTP.Simple
import           RIO.List                   as List
import           System.Directory
import           Text.Blaze                 (toMarkup)
import           Text.Blaze.Renderer.Utf8   (renderMarkup)
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Cursor

run :: RIO App ()
run = do
  cwd <- liftIO getCurrentDirectory
  fetchAndParseAdventOfCodeProblems cwd
  fetchAndParseAdventOfCodeArticles cwd

fetchAndParseAdventOfCodeArticles :: String -> RIO App ()
fetchAndParseAdventOfCodeArticles cwd = do
  let configs = articleConfig cwd <$> [1..25]
  sequenceA_ $ memoizedFetch <$> configs
  sequenceA_ $ parseBody <$> configs

fetchAndParseAdventOfCodeProblems :: String -> RIO App ()
fetchAndParseAdventOfCodeProblems cwd = do
  let configs = adventConfig cwd <$> [1..25]
  sequenceA_ $ memoizedFetch <$> configs
  sequenceA_ $ parseBody <$> configs

{-| fetch the page from the given address and write it to the destination -}
memoizedFetch :: Config -> RIO App ()
memoizedFetch config = do
  exists <- liftIO (doesFileExist $ pathRaw config)
  let showPath = displayShow (pathRaw config)
  let showAddress = displayShow (url config)
  if exists then
    logDebug $ showPath <> " exists, foregoing call to " <> showAddress
  else do
      logInfo $ "caching response from " <> showAddress <> " to " <> showPath
      request <- parseRequest (url config)
      response <- httpLbs request
      let body = getResponseBody response
      writeFileBinary (pathRaw config) (C.toStrict body)

{-| extract the article from the page and write it to the destination -}
parseBody :: Config -> RIO App ()
parseBody config = do
    let destFilePath = pathClean config
    let srcFilePath = pathRaw config
    exists <- liftIO (doesFileExist destFilePath)
    if not exists then do
      liftIO $ withLazyFile (pathRaw config)
        (\file ->
         do
          let cursor = cursorFor file
          let foundStuff = mconcat $ cursor $// (parseContent config) &| extractData
          writeFile destFilePath foundStuff
        )
      logInfo $ displayShow srcFilePath <> " was parsed and written to " <> displayShow destFilePath
      else
      logDebug $ displayShow destFilePath <> " already exists, not writing it."

cursorFor :: LByteString -> Cursor
cursorFor file =
  fromDocument $ parseLBS file

extractData :: Cursor -> LByteString
extractData = renderMarkup . toMarkup . node

data Config = Config
  -- store the raw fetched page data here
  { pathRaw :: FilePath
  -- store the parsed page content here
  , pathClean :: FilePath
  -- parse out the content of interest from the page
  , parseContent :: Cursor -> [Cursor]
  -- the URL to fetch the content from
  , url :: String
  }

-- article-specific config --
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

-- advent-specific config --

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
