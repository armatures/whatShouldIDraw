{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where


import           Import
import           Network.HTTP.Simple
import           System.Directory
import Data.ByteString.Lazy.Char8 as C

run :: RIO App ()
run = do
    p <- liftIO path
    exists <- liftIO (doesFileExist p)
    exitIfExists exists


exitIfExists :: Bool -> RIO App ()
exitIfExists exists =
  if exists then
    -- do
      logInfo "exists" >>
      exitSuccess
  else
      -- logInfo "doesn't exist"
      do
        response <- httpLbs "https://adventofcode.com/2020/day/1"
        let body = getResponseBody response
        filePath <- liftIO path
        writeFileBinary filePath (C.toStrict body)
        logInfo $ displayShow "The status code was: " <> displayShow (getResponseStatusCode response)

path :: IO String
path =
  do
    cwd <- getCurrentDirectory
    return $ cwd ++ "/advents/fetched.html"
