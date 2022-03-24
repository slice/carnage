module Carnage.Search (parseSearchResults, search) where

import Carnage.Models (DbreeID (..), DbreeSearchResult (..), SearchOffset (..))
import Carnage.Root (dbreeRootURL)
import Control.Lens ((^.))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import Network.HTTP.Types (encodePath)
import qualified Network.Wreq as W
import Text.HTML.TagSoup (Tag (TagClose, TagOpen, TagText), parseTags, (~/=))

-- | Parses the HTML response from @dbree.org/s/<query>@ into a list of
-- 'DbreeSearchResult'.
parseSearchResults :: Text -> [DbreeSearchResult]
parseSearchResults html = searchResults
  where
    tags = parseTags html
    list =
      takeWhile (~/= ("<ul class=pagination>" :: String)) $
        dropWhile (~/= ("<ul class=list-group>" :: String)) tags
    searchResults =
      [ DbreeSearchResult
          { uploadID = DbreeID $ T.drop 3 target,
            fileSize = size,
            fileName = name
          }
        | TagOpen "li" [("class", "list-group-item")]
            : TagOpen "span" [("class", "badge")]
            : TagText size
            : TagClose "span"
            : TagOpen "a" [("href", target)]
            : TagText name
            : _ <-
            tails list
      ]

-- | Uses @Network.Wreq@ to search for files from Dbree.
search ::
  -- | The 'Network.Wreq.Options' to use when making the request. Certain
  -- cookies are needed for the request to succeed.
  W.Options ->
  -- | The search query.
  Text ->
  -- | How many initial results to skip (i.e. if @10@ is passed, then the first
  -- ten search results are omitted).
  SearchOffset ->
  IO [DbreeSearchResult]
search options query (SearchOffset offset) = do
  response <- W.getWith options (toString url)
  pure $ parseSearchResults . decodeUtf8 $ response ^. W.responseBody
  where
    pathSegmentsBuilder = encodePath ["s", query <> "&page=" <> show offset] []
    pathSegmentsText = decodeUtf8 . toLazyByteString $ pathSegmentsBuilder
    url = dbreeRootURL <> pathSegmentsText
