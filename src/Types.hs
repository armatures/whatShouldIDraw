{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           RIO
import           RIO.Process
import           Text.XML.Cursor

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


data Config = Config
  -- store the raw fetched page data here
  { pathRaw      :: FilePath
  -- store the parsed page content here
  , pathClean    :: FilePath
  -- parse out the content of interest from the page
  , parseContent :: Cursor -> [Cursor]
  -- the URL to fetch the content from
  , url          :: String
  }
