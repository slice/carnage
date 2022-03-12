module Main where

import Carnage
import Carnage.Discovery
import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (..), Value, eitherDecode', object, withObject, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import Network.HTTP.Client (Cookie (..), createCookieJar)
import qualified Network.Wreq as W
import Polysemy
import System.Directory (doesFileExist)

runSeenIDStoreFile ::
  Member (Embed IO) r =>
  FilePath ->
  Sem (SeenIDStore ': r) a ->
  Sem r a
runSeenIDStoreFile path = interpret $ \case
  ReadSeenIDs -> do
    text <- embed $ decodeUtf8 <$> readFileBS path
    pure . HS.fromList $ DbreeID <$> lines text
  WriteSeenIDs ids -> do
    let text = unlines $ unDbreeID <$> HS.toList ids
     in embed $ writeFileBS path $ encodeUtf8 text

runDbreeSearchWreq ::
  Member (Embed IO) r =>
  W.Options ->
  Sem (DbreeSearch ': r) a ->
  Sem r a
runDbreeSearchWreq options = interpret $ \case
  SearchDbree query searchOffset@(SearchOffset offset) ->
    embed $ putStrLn ("searching for " <> show query <> " (offset=" <> show offset <> ") ...") >> search options query searchOffset

data Config = Config
  { ddg1Cookie :: Text,
    ddg2Cookie :: Text,
    ddgidCookie :: Text,
    queries :: [Text],
    userAgent :: Text,
    seenFilePath :: FilePath,
    discordWebhookUrl :: Text
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .: "ddg1"
      <*> v .: "ddg2"
      <*> v .: "ddgid"
      <*> v .: "queries"
      <*> v .: "userAgent"
      <*> v .: "seenFilePath"
      <*> v .: "discordWebhookUrl"

readConfig :: FilePath -> IO Config
readConfig path = do
  text <- readFileLBS path
  either fail pure $ eitherDecode' text

postToWebhook :: Text -> Text -> [DbreeSearchResult] -> IO ()
postToWebhook discordWebhookUrl query results = bodyIO >>= void . W.post (toString discordWebhookUrl)
  where
    plural = if length results == 1 then "" else "s"
    header = "Detected " <> show (length results) <> " new file" <> plural <> " for query `" <> query <> "`."

    markdownLink text url = "[" <> text <> "](" <> url <> ")"
    markdownLinkWithoutEmbed text url = markdownLink text ("<" <> url <> ">")

    compactListing =
      unlines
        . fmap
          ( \sr@DbreeSearchResult {fileName, fileSize} ->
              "\8226 "
                <> markdownLinkWithoutEmbed fileName (searchResultUrl sr)
                <> " (`"
                <> fileSize
                <> "`)"
          )

    truncateTo :: Int -> Text -> Text
    truncateTo maxLength text =
      if T.length text > maxLength
        then T.take (maxLength - 1) text <> "\8230"
        else text

    bodyIO :: IO Value
    bodyIO =
      if length results > 10
        then pure $ object ["content" .= truncateTo 2000 (header <> "\n\n" <> compactListing results)]
        else (\embeds -> object ["content" .= header, "embeds" .= embeds]) <$> traverse encodeSearchResultEmbed results

main :: IO ()
main =
  do
    configFilePath <- getArgs >>= maybe (fail "missing config file path argument") pure . listToMaybe
    Config {ddg1Cookie, ddg2Cookie, ddgidCookie, queries, userAgent, seenFilePath, discordWebhookUrl} <- readConfig configFilePath

    unlessM (doesFileExist seenFilePath) (writeFileBS seenFilePath "")

    now <- C.getCurrentTime
    let makeCookie name value = Cookie name value (C.addUTCTime (C.nominalDay * 30) now) "dbree.org" "/" now now True True False False
        cookieJar =
          createCookieJar
            [ makeCookie "__ddg1" $ encodeUtf8 ddg1Cookie,
              makeCookie "__ddg2" $ encodeUtf8 ddg2Cookie,
              makeCookie "__ddgid" $ encodeUtf8 ddgidCookie
            ]
        options =
          W.defaults
            & W.header "User-Agent" .~ [encodeUtf8 userAgent]
            & W.cookies ?~ cookieJar

    queryResults <- runQueriesIO options seenFilePath queries

    traverse_
      ( \(query, files) ->
          putTextLn ("*** for query " <> show query <> ", discovered " <> show (length files) <> " file(s)")
            >> traverse_ printSearchResult files
            >> unless (null files) (postToWebhook discordWebhookUrl query files)
      )
      (HM.toList queryResults)
  where
    printSearchResult :: DbreeSearchResult -> IO ()
    printSearchResult DbreeSearchResult {fileName, fileSize} =
      putTextLn $ "  - " <> fileName <> " (" <> fileSize <> ")"

    runQueriesIO :: W.Options -> FilePath -> [Text] -> IO (HashMap Text [DbreeSearchResult])
    runQueriesIO options path queries =
      processQueries queries
        & runSeenIDStoreFile path
        & runDbreeSearchWreq options
        & runM
