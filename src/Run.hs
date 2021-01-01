{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where


import           Import
import           Network.HTTP.Simple
import           RIO.FilePath        ((</>))
import           System.Directory

run :: RIO App ()
run = do
    p <- liftIO path
    exists <- liftIO (doesFileExist p)
    exitIfExists exists

    -- logInfo $ displayShow exists
  -- logInfo . L.take 500 =<< httpLBS url


exitIfExists :: Bool -> RIO App ()
exitIfExists exists =
  if exists then
      logInfo (displayShow "exists")
      -- exitSuccess
  else
      logInfo $ displayShow "doesn't exist"
      -- do
      --   (response <- httpLbs "http://httpbin.org/get"
      --   logInfo $ displayShow "The body: " <> (displayShow . getResponseBody ) response
      --   logInfo $ displayShow "The status code was: " <> displayShow (getResponseStatusCode response)
      --     )      -- print $ getResponseHeader "Content-Type" response
      -- logInfo $ displayShow $ getResponseBody response

path =
  do
    cwd <- getCurrentDirectory
    return $ cwd ++ "/fetched.html"
