{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where


import           Import
import           Network.HTTP.Simple
import           System.Directory
import Data.ByteString.Lazy.Char8 as C

run :: RIO App ()
run = do
    cwd <- liftIO getCurrentDirectory
    let args = makeArg cwd <$> [1..25]
    traverse_ (uncurry memoizedFetch) args

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


makeArg :: FilePath -> Int -> (String, String)
makeArg cwd i =
  ("https://adventofcode.com/2020/day/" <> show i
  , cwd ++ "/advents/" <> show i <>".html")
