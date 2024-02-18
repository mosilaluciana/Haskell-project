module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  db <- DB.save DB.empty
  case db of
    (Error lde) -> return ()
    (Success db) -> print "Success"


-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    Result.Error _ -> putStrLn "Failed to load DB"
    Result.Success entries ->
      case DB.findFirst (\x -> entryId x == getOptId getOpts) entries of
        Nothing -> putStrLn $ "No matching entry with id " ++ show (getOptId getOpts) ++ " in the database."
        Just entry -> putStrLn (entrySnippet entry)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    Result.Error _ -> putStrLn "Failed to load DB"
    Result.Success db' ->
      let matchingEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db'
      in if null matchingEntries
           then putStrLn "No entries found."
           else mapM_ (putStrLn . show . FmtEntry) matchingEntries

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  db <- DB.load
  entries <- readFile (addOptFilename addOpts)
  case db of
    Result.Error _ -> putStrLn "Failed to load DB"
    Result.Success db' ->
      case DB.findFirst (\x -> entrySnippet x == entries) db' of
        Just id -> putStrLn $ "Entry with this content already exists: \n" ++ show (FmtEntry id)
        Nothing -> do
          DB.modify $
            DB.insertWith
              ( \id' ->
                  Entry
                    { entryId = id',
                      entrySnippet = entries,
                      entryFilename = addOptFilename addOpts,
                      entryLanguage = addOptLanguage addOpts,
                      entryDescription = addOptDescription addOpts,
                      entryTags = addOptTags addOpts
                    }
              )
          return ()




-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
