{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where


import           Data.ByteString.Lazy.Char8 as C
import           Data.Text.Encoding
import           Import
import           Network.HTTP.Simple
import           RIO.List                   as List
import           RIO.Text                   as T
import           System.Directory
import           Text.Blaze                 (toMarkup)
import           Text.Blaze.Renderer.Utf8   (renderMarkup)
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Cursor

run :: RIO App ()
run = do
  cwd <- liftIO getCurrentDirectory
  let urls = adventUrl <$> [1..25]
  let paths = adventFilePath cwd <$> [1..25]
  sequenceA_ $ List.zipWith memoizedFetch urls paths
  parseBody $ adventFilePath cwd 1

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
      logInfo $ displayShow "The status code was: " <> displayShow (getResponseStatusCode response)


adventFilePath cwd i
  = cwd ++ "/advents/" <> show i <>".html"

adventUrl i =
  "https://adventofcode.com/2020/day/" <> show i
{-| clean the up the article, taking only what you want to read -}

parseBody :: FilePath -> RIO App ()
parseBody filePath =
    liftIO $ withLazyFile filePath
      (\file ->
        do
          let cursor = cursorFor file
          let foundStuff = cursor $// findNodes &| extractData
          putStrLn "found nodes:"
          traverse_ (putStrLn . C.pack . T.unpack) foundStuff
          return ()
      )
  >> return ()

cursorFor :: C.ByteString -> Cursor
cursorFor file =
  fromDocument $ parseLBS file

findNodes :: Cursor -> [Cursor]
findNodes = element "main" &/ element "article"

extractData :: Cursor -> Text
extractData = decodeASCII . toStrict . renderMarkup . toMarkup . node
