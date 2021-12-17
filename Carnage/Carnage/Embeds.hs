module Carnage.Embeds where

import Carnage.Models
import Carnage.Root (dbreeRootURL)
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

-- | Returns a link to a Dbree search result.
searchResultUrl :: DbreeSearchResult -> Text
searchResultUrl DbreeSearchResult {uploadID = DbreeID uploadID} =
  dbreeRootURL <> "/v/" <> uploadID

-- | Encodes a 'DbreeSearchResult' into a Discord embed as JSON.
encodeSearchResultEmbed :: DbreeSearchResult -> IO AE.Value
encodeSearchResultEmbed sr@DbreeSearchResult {fileName, fileSize} = do
  time <- getCurrentTime
  pure $
    AE.object
      [ "title" .= fileName,
        "url" .= searchResultUrl sr,
        "timestamp" .= iso8601Show time,
        ("footer", AE.object ["text" .= fileSize])
      ]
