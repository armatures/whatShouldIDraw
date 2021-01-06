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
  let urls = adventUrl <$> [1..25]
  let srcPaths = adventFilePath cwd <$> [1..25]
  let destPaths = adventFilePathClean cwd <$> [1..25]
  sequenceA_ $ List.zipWith memoizedFetch urls srcPaths
  sequenceA_ $ List.zipWith parseBody srcPaths destPaths

memoizedFetch :: String -> FilePath -> RIO App ()
memoizedFetch address path = do
  exists <- liftIO (doesFileExist $ path)
  let showPath = displayShow path
  let showAddress = displayShow address
  if exists then
    logDebug $ showPath <> " exists, foregoing call to " <> showAddress
  else do
      logInfo $ "caching response from " <> showAddress <> " to " <> showPath
      request <- parseRequest address
      response <- httpLbs request
      let body = getResponseBody response
      writeFileBinary path (C.toStrict body)

adventFilePath :: FilePath -> Int -> FilePath
adventFilePath cwd i
  = cwd ++ "/advents/" <> show i <>".html"

adventFilePathClean :: FilePath -> Int -> FilePath
adventFilePathClean cwd i
  = cwd ++ "/adventsClean/" <> show i <>".html"

adventUrl :: Int -> String
adventUrl i =
  "https://adventofcode.com/2020/day/" <> show i

{-| extract the article from the page and write it to the destination -}
parseBody :: FilePath -> FilePath -> RIO App ()
parseBody srcFilePath destFilePath=
    liftIO $ withLazyFile srcFilePath
      (\file ->
        do
          let cursor = cursorFor file
          let foundStuff = mconcat $ cursor $// findNodes &| extractData
          writeFile destFilePath foundStuff
      )
  >> return ()

cursorFor :: LByteString -> Cursor
cursorFor file =
  fromDocument $ parseLBS file

findNodes :: Cursor -> [Cursor]
findNodes = element "main" &/ element "article"

extractData :: Cursor -> LByteString
extractData = renderMarkup . toMarkup . node
