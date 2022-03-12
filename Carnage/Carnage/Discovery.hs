module Carnage.Discovery
  ( -- * File Discovery
    discoverNewFiles,
    processQueries,
    dbreePageSize,

    -- * Effects
    DbreeSearch (..),
    searchDbree,
    SeenIDStore (..),
    readSeenIDs,
    writeSeenIDs,
  )
where

import Carnage.Models (DbreeID, DbreeSearchResult (..), SearchOffset (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Polysemy

-- | The number of search results present per page.
dbreePageSize :: Int
dbreePageSize = 10

data DbreeSearch m a where
  SearchDbree :: Text -> SearchOffset -> DbreeSearch m [DbreeSearchResult]

makeSem ''DbreeSearch

data SeenIDStore m a where
  ReadSeenIDs :: SeenIDStore m (HashSet DbreeID)
  WriteSeenIDs :: HashSet DbreeID -> SeenIDStore m ()

makeSem ''SeenIDStore

-- | Discover all new files matching a query given a set of seen DBREE upload IDs.
--
-- A set of "already seen" DBREE upload IDs is consulted in other to drive
-- searches past the initial page (vital to ensuring that new files past the
-- first page are noticed). If all of the files on a page are new (i.e., not
-- present in the "already seen" ID set), then the next page is fetched up to
-- 10 times.
discoverNewFiles ::
  Member DbreeSearch r =>
  -- | The set of already seen file IDs.
  HashSet DbreeID ->
  -- | The query to make when searching for new files.
  Text ->
  Sem r [DbreeSearchResult]
discoverNewFiles alreadySeen query = go (SearchOffset 0) []
  where
    go :: Member DbreeSearch r => SearchOffset -> [DbreeSearchResult] -> Sem r [DbreeSearchResult]
    go (SearchOffset offset) seenSoFar = do
      files <- searchDbree query (SearchOffset offset)

      let fetchedIDs = HS.map uploadID $ HS.fromList files
          familiarIDs = HS.intersection fetchedIDs alreadySeen

      -- if none of the files we've just fetched are familiar, fetch more (at most, 10 times)
      if HS.null familiarIDs && offset < (dbreePageSize * 9)
        then go (SearchOffset $ offset + dbreePageSize) (seenSoFar ++ files)
        else -- otherwise, return all the new files
          pure $ seenSoFar ++ filter (not . (`HS.member` alreadySeen) . uploadID) files

-- | Discover all new files from a list of queries, saving the set of all newly
-- seen files to a store.
--
-- The modus operandi of this function is to handle the common use case of making
-- multiple queries gracefully. There are problems that may arise from handling
-- "already seen" files naively. Should a query surface enough new files that
-- themselves appear in a later query (due to both queries matching), then it's
-- possible that the latter query will miss new files if there were enough
-- results from the former query to offset them into the next page. This is due
-- to the heuristics of the 'discoverNewFiles' function, which will stop
-- fetching pages should an entire page of files appear in the "already seen"
-- set.
--
-- If one were to process these queries completely separately, then files from
-- the former query would be marked as seen and would possibly present the
-- illusion of there being no new files at all, should they be obscured by a
-- full page of seen files. Therefore, this function passes the same set of
-- seen file IDs when making queries and takes care to deduplicate them
-- afterwards.
processQueries ::
  Member DbreeSearch r =>
  Member SeenIDStore r =>
  -- | The search queries to make.
  [Text] ->
  Sem r (HashMap Text [DbreeSearchResult])
processQueries queries = do
  alreadySeenIDs <- readSeenIDs
  queryResults <- traverse (discoverNewFiles alreadySeenIDs) queries
  writeSeenIDs $ HS.union (HS.fromList $ uploadID <$> join queryResults) alreadySeenIDs
  pure $ HM.fromList . zip queries . dedupPreceding $ queryResults
  where
    -- We need to deduplicate the results from each query, but in such a way
    -- that only files from previous queries are considered to be seen.
    dedupPreceding :: [[DbreeSearchResult]] -> [[DbreeSearchResult]]
    dedupPreceding = go HS.empty
      where
        go seen (results : tl) = filteredSearchResults : go newSeen tl
          where
            filteredSearchResults = filter (not . (`HS.member` seen)) results
            newSeen = HS.union seen (HS.fromList results)
        go _ [] = []
