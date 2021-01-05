{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where


import           Data.ByteString.Lazy.Char8 as C
import           Data.Text.Encoding
import           Import
import           Network.HTTP.Simple
import           RIO.Text                   as T
import           System.Directory
import           Text.Blaze                 (toMarkup)
import           Text.Blaze.Renderer.Utf8   (renderMarkup)
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Cursor

run :: RIO App ()
run = do
  cwd <- liftIO getCurrentDirectory
  let args = makeArg cwd <$> [1..25]
  traverse_ (uncurry memoizedFetch) args
  parseBody $ snd (makeArg cwd 1)

memoizedFetch :: String -> FilePath -> RIO App ()
memoizedFetch address path = do
  exists <- liftIO (doesFileExist $ path)
  let showPath = displayShow path
  let showAddress = displayShow address
  if exists then
      logInfo $ showPath <> " exists, foregoing call to " <> showAddress
  else do
      logInfo $ "caching response from " <> showAddress <> " to " <> showPath
      request <- parseRequest address
      response <- httpLbs request
      let body = getResponseBody response
      writeFileBinary path (C.toStrict body)
      logInfo $ displayShow "The status code was: " <> displayShow (getResponseStatusCode response)


makeArg :: FilePath -> Int -> (String, FilePath)
makeArg cwd i =
  ("https://adventofcode.com/2020/day/" <> show i
  , cwd ++ "/advents/" <> show i <>".html")

parseBody :: FilePath -> RIO App ()
parseBody filePath =
    liftIO $ withLazyFile filePath
      (\file ->
        do
          let cursor = cursorFor file
          -- processData $
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
extractData = decodeASCII . toStrict . renderMarkup . toMarkup . node -- T.concat . content
