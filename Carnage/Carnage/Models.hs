module Carnage.Models
  ( DbreeID (..),
    DbreeSearchResult (..),
    SearchOffset (..),
  )
where

newtype DbreeID = DbreeID {unDbreeID :: Text}
  deriving (Show, Hashable, Eq)

data DbreeSearchResult = DbreeSearchResult
  { uploadID :: DbreeID,
    fileSize :: Text,
    fileName :: Text
  }
  deriving (Show)

instance Hashable DbreeSearchResult where
  hashWithSalt salt DbreeSearchResult {uploadID} = hashWithSalt salt uploadID

instance Eq DbreeSearchResult where
  a == b = uploadID a == uploadID b

newtype SearchOffset = SearchOffset Int
