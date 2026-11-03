-- | Fetches e-mails with the `Gmail` effect, and converts them to plain text.
module Expenses.Gmail (
  fetchEmails,
  toEmail,
  htmlToText,
  normalizeText,
  maxBodyChars,
) where

import CustomPrelude
import Data.ByteString.Base64.URL qualified as Base64
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Log (Log)
import Expenses.Effects.Gmail (Gmail, RefreshToken)
import Expenses.Effects.Gmail qualified as Gmail
import Expenses.Email (Email (..))
import Gmail (GmailMessage (..), MessageHeader (..), MessageList (..), MessagePart (..), MessageRef (..))
import Log (logAttention_)
import Text.HTML.TagSoup (Tag (..), parseTags)

-- | E-mails' bodies are cut to this length, so that a long e-mail doesn't take up the whole prompt.
maxBodyChars :: Int
maxBodyChars = 10_000

{- | Fetches the e-mails that match a Gmail search query, newest first.

Fetches at most `maxMessages` e-mails, and logs a warning when there are more.
-}
fetchEmails ::
  (Gmail :> es, Log :> es) =>
  -- | The Gmail address.
  Text ->
  RefreshToken ->
  -- | The Gmail search query, e.g. "newer_than:14d".
  Text ->
  -- | The maximum number of e-mails.
  Int ->
  Eff es [Email]
fetchEmails account refreshToken query maxMessages = do
  token <- Gmail.getAccessToken refreshToken
  (messageIds, hasMore) <- listIds token Nothing []
  when hasMore do
    logAttention_ [i|[Gmail] More than #{maxMessages} e-mails in #{account} match '#{query}'. Only the newest #{maxMessages} are used.|]
  forM messageIds \messageId ->
    Gmail.getMessage token messageId <&> toEmail account
 where
  -- Follows the pages until there are no more, or until we have `maxMessages` ids.
  -- Returns the ids, and whether there are more.
  listIds token pageToken acc = do
    page <- Gmail.listMessages token query pageToken
    let ids = acc <> maybe [] (map (.id)) page.messages
    if
      | length ids > maxMessages -> pure (take maxMessages ids, True)
      | length ids == maxMessages -> pure (ids, isJust page.nextPageToken)
      | otherwise -> case page.nextPageToken of
          Nothing -> pure (ids, False)
          Just next -> listIds token (Just next) ids

{- | Converts a message to an `Email`.

The body is the first "text/plain" part. If there is none, it is the first "text/html" part,
converted to text. Attachments are ignored.
-}
toEmail :: Text -> GmailMessage -> Email
toEmail account GmailMessage{id = messageId, payload} =
  Email
    { account
    , messageId
    , from = header "From"
    , subject = header "Subject"
    , date = header "Date"
    , body = T.take maxBodyChars $ normalizeText bodyText
    }
 where
  header :: Text -> Text
  header name = fromMaybe "" $ findHeader name payload

  bodyText :: Text
  bodyText =
    case findPart "text/plain" payload of
      Just part -> decodePart part
      Nothing -> maybe "" (htmlToText . decodePart) (findPart "text/html" payload)

findHeader :: Text -> MessagePart -> Maybe Text
findHeader name part =
  part.headers
    & find (\h -> T.toCaseFold h.name == T.toCaseFold name)
    <&> (.value)

-- | Finds the first part with the given MIME type, depth-first. Ignores attachments.
findPart :: Text -> MessagePart -> Maybe MessagePart
findPart mimeType part
  | not (T.null part.filename) = Nothing
  | T.toCaseFold part.mimeType == mimeType && isJust part.bodyData = Just part
  | otherwise = asum (map (findPart mimeType) part.parts)

-- | Decodes a part's base64url data, using the charset in its "Content-Type" header.
decodePart :: MessagePart -> Text
decodePart part =
  case T.toCaseFold <$> charset of
    Just cs | cs `elem` ["iso-8859-1", "latin1", "windows-1252", "us-ascii"] -> TE.decodeLatin1 bytes
    _ -> TE.decodeUtf8With lenientDecode bytes
 where
  bytes = Base64.decodeBase64Lenient $ encodeUtf8 $ fromMaybe "" part.bodyData

  -- E.g. `text/plain; charset="ISO-8859-1"`
  charset :: Maybe Text
  charset = do
    contentType <- findHeader "Content-Type" part
    param <- contentType & T.splitOn ";" & map T.strip & find (T.isPrefixOf "charset=" . T.toCaseFold)
    pure $ T.dropAround (== '"') $ T.drop (T.length "charset=") param

{- | Converts HTML to text: keeps the text, ends a line at each block element,
and drops `<head>`, `<script>` and `<style>`. `tagsoup` decodes the HTML entities.
-}
htmlToText :: Text -> Text
htmlToText = T.concat . go . parseTags
 where
  go :: [Tag Text] -> [Text]
  go = \case
    [] -> []
    TagOpen name _ : rest
      | lower name `elem` ["head", "script", "style", "title"] ->
          go $ drop 1 $ dropWhile (not . isClose name) rest
    TagOpen name _ : rest | isBlock name -> "\n" : go rest
    TagClose name : rest | isBlock name -> "\n" : go rest
    TagOpen name _ : rest | isCell name -> " " : go rest
    TagText text : rest -> text : go rest
    _ : rest -> go rest

  isClose :: Text -> Tag Text -> Bool
  isClose name = \case
    TagClose closeName -> lower closeName == lower name
    _ -> False

  isBlock name = lower name `elem` ["br", "p", "div", "tr", "li", "table", "ul", "ol", "h1", "h2", "h3", "h4", "h5", "h6"]
  isCell name = lower name `elem` ["td", "th"]
  lower = T.toLower

{- | Makes the text shorter, without changing its meaning:
  * replaces non-breaking spaces with spaces, and removes invisible characters
    (e.g. the zero-width characters that some e-mails use to hide their preview text),
  * collapses whitespace in each line,
  * collapses consecutive empty lines, and removes empty lines at the start and end.
-}
normalizeText :: Text -> Text
normalizeText text =
  text
    & T.map (\c -> if c == '\xA0' then ' ' else c)
    & T.filter (not . isInvisible)
    & T.lines
    & map (T.unwords . T.words)
    & collapseEmptyLines
    & T.intercalate "\n"
 where
  isInvisible c = c `elem` ['\x200B', '\x200C', '\x200D', '\xFEFF', '\x034F', '\x00AD'] || Char.generalCategory c == Char.Control && c /= '\n'

  collapseEmptyLines :: [Text] -> [Text]
  collapseEmptyLines =
    dropWhileEnd T.null . dropWhile T.null . dedup
   where
    dedup = \case
      "" : "" : rest -> dedup ("" : rest)
      line : rest -> line : dedup rest
      [] -> []

  dropWhileEnd p = reverse . dropWhile p . reverse
