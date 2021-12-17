import Carnage.Discovery
import Carnage.Internal.Squeeze
import Carnage.Models
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.HashSet as HS
import qualified Polysemy as P
import qualified Polysemy.State as P
import Test.Hspec

runDbreeSearchPure ::
  HashMap Text [DbreeSearchResult] ->
  P.Sem (DbreeSearch ': r) a ->
  P.Sem r a
runDbreeSearchPure lookup = P.interpret $ \case
  SearchDbree query (SearchOffset offset) ->
    pure . take dbreePageSize . drop offset $ HM.findWithDefault [] query lookup

runSeenIDStore ::
  P.Sem (SeenIDStore ': r) a ->
  P.Sem (P.State (HashSet DbreeID) ': r) a
runSeenIDStore = P.reinterpret $ \case
  ReadSeenIDs -> P.get
  WriteSeenIDs seenIDs -> P.put seenIDs

main :: IO ()
main = hspec $ do
  describe "Carnage.Squeeze" $ do
    it "squeezes" $ do
      let squeeze = squeezeBy (==) :: ([Int] -> [Squeezed Int])
      squeeze [0] `shouldBe` [Unique 0]
      squeeze [0, 0] `shouldBe` [Squeezed 2 0]
      squeeze [0, 0, 5] `shouldBe` [Squeezed 2 0, Unique 5]
      squeeze [0, 5, 5, 5, 0, 5] `shouldBe` [Unique 0, Squeezed 3 5, Unique 0, Unique 5]
      squeeze [0, 5, 5, 0, 5, 5, 5] `shouldBe` [Unique 0, Squeezed 2 5, Unique 0, Squeezed 3 5]
  describe "Carnage.Discovery" $ do
    describe "discoverNewFiles" $ do
      it "discovers new files" $ do
        let results = [sr "bar", sr "quux"]
        discoverWithResults HS.empty results `shouldBe` results

      it "ignores already seen files" $ do
        let alreadySeen = [sr "as1", sr "as2", sr "as3"]
            newResults = [sr "new1", sr "new2"]
        discoverWithResults
          (fromList $ uploadID <$> alreadySeen)
          (newResults ++ alreadySeen)
          `shouldBe` newResults

      it "doesn't fetch more files when the first page is entirely familiar" $ do
        let results = replicate dbreePageSize (sr "old") ++ [sr "new1", sr "new2"]
        discoverWithResults (fromList [DbreeID "old"]) results `shouldBe` []

      it "fetches more files when the first page is entirely new" $ do
        let results = [sr $ "f" <> show n | n <- [(1 :: Int) .. 30]]
        discoverWithResults HS.empty results `shouldBe` results

      it "fetches up to 10 pages when the first page is entirely new" $ do
        let results = [sr $ "g" <> show n | n <- [(1 :: Int) .. 105]]
            discovered = discoverWithResults HS.empty results
        length discovered `shouldBe` 100
    describe "processQueries" $ do
      it "deduplicates seen files from previous queries" $ do
        let uniqueFooResults = replicateSR 10 "unique foo"
            uniqueFooBarResults = replicateSR 5 "unique foo bar"
            -- this is enough to push the 'fooBarResults' off the first page
            fooResults = replicateSR 20 "foo"
            fooBarResults = replicateSR 15 "foo bar"
            predeterminedResults =
              HM.fromList
                [ ("foo", uniqueFooResults ++ fooResults),
                  -- the second query include results from the first one, which
                  -- should be deduplicated
                  ("foo bar", fooResults ++ fooBarResults ++ uniqueFooBarResults)
                ]
            processedResults =
              HM.fromList
                [ ("foo", uniqueFooResults ++ fooResults),
                  ("foo bar", fooBarResults ++ uniqueFooBarResults)
                ]
            allIDs = HS.unions . fmap ids $ [uniqueFooResults, uniqueFooBarResults, fooResults, fooBarResults]
        ( processQueries ["foo", "foo bar"]
            & runDbreeSearchPure predeterminedResults
            & runSeenIDStore
            & P.runState HS.empty
            & P.run
          )
          `shouldBe` (allIDs, processedResults)
  where
    -- Quickly create a placeholder 'DbreeSearchResult'.
    sr :: Text -> DbreeSearchResult
    sr text = DbreeSearchResult (DbreeID text) "" ""

    ids :: [DbreeSearchResult] -> HashSet DbreeID
    ids = HS.fromList . fmap uploadID

    -- Quickly create multiple placeholder 'DbreeSearchResult's.
    replicateSR :: Int -> Text -> [DbreeSearchResult]
    replicateSR n name = [sr $ name <> " " <> show i | i <- [(1 :: Int) .. n]]

    -- Discover files given a set of already seen IDs with a predetermined list
    -- of search results.
    discoverWithResults :: HashSet DbreeID -> [DbreeSearchResult] -> [DbreeSearchResult]
    discoverWithResults seen results =
      discoverNewFiles seen "query"
        & runDbreeSearchPure (fromList [("query", results)])
        & P.run
