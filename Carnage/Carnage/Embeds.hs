module Carnage.Embeds where

import Carnage.Models
import Carnage.Root (dbreeRootURL)
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

-- | Encodes a 'DbreeSearchResult' into a Discord embed as JSON.
encodeSearchResultEmbed :: DbreeSearchResult -> IO AE.Value
encodeSearchResultEmbed DbreeSearchResult {fileName, fileSize, uploadID = DbreeID uploadID} = do
  time <- getCurrentTime
  pure $
    AE.object
      [ "title" .= fileName,
        "url" .= fileURL,
        "timestamp" .= iso8601Show time,
        ("footer", AE.object ["text" .= fileSize])
      ]
  where
    fileURL = dbreeRootURL <> "/v/" <> uploadID
