module Mustache (render) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Argonaut (class EncodeJson, Json, caseJson, caseJsonObject, encodeJson)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either, fromRight)
import Data.Foldable (class Foldable, foldMap, foldl, intercalate)
import Data.Int (fromNumber)
import Data.List.Types as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Utils (trimEnd)
import Debug.Trace (spy, traceM)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try, unParser)
import Text.Parsing.StringParser.CodeUnits (alphaNum, anyChar, anyLetter, char, eof, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between, choice, lookAhead, many, many1, many1Till, manyTill, (<?>))

data Doc
  = PlainText String
  | EscapedVarTag String
  -- | All variables are HTML escaped by default. If you want unescaped HTML,
  -- | use the triple mustache: {{{name}}}
  | UnescapedVarTag String
  | Section String (Array Doc)
  | InvertedSection String (Array Doc)
  | EmptyDoc

render :: String -> Json -> Either String String
render template json =
  lmap show do
    { result: docs } <- unParser docsParser { pos: 0, str: template }
    -- docs <- runParser docsParser template
    pure $ renderDocs docs [ json ]

renderDocs :: Array Doc -> Array Json -> String
renderDocs docs jsons = foldMap (renderDoc jsons) docs

renderDoc :: Array Json -> Doc -> String
renderDoc jsons doc = case doc of
  PlainText s -> s
  EscapedVarTag key -> fromMaybe (rawTag "{{" "}}" key) $ showTag key jsons
  UnescapedVarTag key -> fromMaybe (rawTag "{{{" "}}}" key) $ showTag key jsons
  Section key innerDocs -> fromMaybe mempty $ getTag key jsons <#> \t -> renderSection t jsons key innerDocs
  InvertedSection key innerDocs -> case getTag key jsons of
    Just jsonVal -> renderInvertedSection jsonVal jsons key innerDocs
    Nothing -> renderDocs innerDocs jsons
  EmptyDoc -> mempty

renderSection :: Json -> Array Json -> String -> Array Doc -> String
renderSection json jsons key innerDocs =
  caseJson
    (const mempty)
    (renderInnerSectionWhen identity)
    (renderInnerSectionWhen $ (/=) 0.0)
    (renderInnerSectionWhen $ not <<< S.null)
    (renderArraySection innerDocs jsons)
    (renderObjectSection innerDocs jsons)
    json
  where
  renderInnerSectionWhen :: forall a. EncodeJson a => (a -> Boolean) -> a -> String
  renderInnerSectionWhen p a = renderSectionWhen p (encodeJson >>> flip A.cons jsons >>> renderDocs innerDocs) a

renderInvertedSection :: Json -> Array Json -> String -> Array Doc -> String
renderInvertedSection json jsons key innerDocs =
  caseJson
    (const mempty)
    (renderInnerSectionWhen not)
    (renderInnerSectionWhen $ (==) 0.0)
    (renderInnerSectionWhen S.null)
    (renderInvertedArraySection innerDocs jsons)
    (const mempty)
    json
  where
  renderInnerSectionWhen :: forall a. EncodeJson a => (a -> Boolean) -> a -> String
  renderInnerSectionWhen p a = renderSectionWhen p (encodeJson >>> flip A.cons jsons >>> renderDocs innerDocs) a

renderSectionWhen :: forall a. EncodeJson a => (a -> Boolean) -> (Json -> String) -> a -> String
renderSectionWhen p r a =
  if p a then
    encodeJson a # r
  else
    mempty

renderArraySection :: Array Doc -> Array Json -> Array Json -> String
renderArraySection innerDocs jsons = foldMap $ renderDocs innerDocs <<< flip A.cons jsons

renderInvertedArraySection :: Array Doc -> Array Json -> Array Json -> String
renderInvertedArraySection innerDocs jsons [] = renderDocs innerDocs jsons

renderInvertedArraySection _ _ _ = mempty

renderObjectSection :: Array Doc -> Array Json -> O.Object Json -> String
renderObjectSection innerDocs jsons = encodeJson >>> flip A.cons jsons >>> renderDocs innerDocs

rawTag :: String -> String -> String -> String
rawTag opening closing key = opening <> key <> closing

getTag :: String -> Array Json -> Maybe Json
getTag tag = foldl go Nothing
  where
  go r@(Just _) _ = r

  go Nothing json = lookupTag json

  lookupTag json = caseJsonObject Nothing (O.lookup tag) json

showTag :: String -> Array Json -> Maybe String
showTag tag jsons = getTag tag jsons <#> showJson

showJson :: Json -> String
showJson json =
  caseJson
    mempty
    show
    showNumber
    identity
    showJsonArray
    (const "[object Object]")
    json

showNumber :: Number -> String
showNumber n = case fromNumber n of
  Just ni -> show ni
  Nothing -> show n

showJsonArray :: Array Json -> String
showJsonArray xs = map showJson xs # intercalate ","

sectionParser :: Parser Doc
sectionParser = do
  symbol "{{#"
  sectionKey <- keyParser
  symbol "}}"
  innerDocs <- innerDocsParser <#> A.fromFoldable
  symbol $ "{{/" <> sectionKey <> "}}"
  pure $ Section sectionKey innerDocs
  where
  innerDocsParser =
    manyTill (choice [ defer \_ -> sectionParser, defer \_ -> invertedSectionParser, tagParser, plainTextParser ])
      $ (lookAhead $ symbol "{{/")

invertedSectionParser :: Parser Doc
invertedSectionParser = do
  symbol "{{^"
  sectionKey <- keyParser
  symbol "}}"
  newLine <|> pure unit
  innerDocs <- innerDocsParser <#> A.fromFoldable
  traceM innerDocs
  skipSpaces
  symbol $ "{{/" <> sectionKey <> "}}"
  newLine <|> pure unit
  pure $ InvertedSection sectionKey innerDocs
  where
  sectionEnd = lookAhead $ spaces *> symbol "{{/"

  innerDocsParser =
    manyTill
      (choice [ defer \_ -> invertedSectionParser, defer \_ -> sectionParser, tagParser, plainTextParser ])
      sectionEnd

newLine :: Parser Unit
newLine = void $ choice [ char '\n', char '\r' ]

space :: Parser Unit
space = void $ choice [ char '\t', char ' ' ]

spaces :: Parser Unit
spaces = void $ many space

docsParser :: Parser (Array Doc)
docsParser = A.fromFoldable <$> docsParser'
  where
  docsParser' =
    many1Till
      ( choice
          [ eof $> EmptyDoc
          , sectionParser
          , invertedSectionParser
          , tagParser
          , plainTextParser
          ]
      )
      eof

fromFoldableChars :: forall f. Foldable f => f Char -> Doc
fromFoldableChars = PlainText <<< fromCharArray <<< A.fromFoldable

plainTextParser :: Parser Doc
plainTextParser = fromFoldableChars <$> many1Till anyChar endings
  where
  endings = choice $ lookAhead <$> endSymbols

  endSymbols =
    [ eof
    , symbol "{{^"
    , symbol "{{#"
    , symbol "{{/"
    , symbol "{{"
    , many1 space *> symbol "{{#"
    , many1 space *> symbol "{{^"
    , many1 space *> symbol "{{/"
    ]

newLineAndSpaces :: Parser Unit
newLineAndSpaces = newLine *> spaces

any1CharTill :: forall f a. Foldable f => Functor f => f (Parser a) -> Parser String
any1CharTill parsers = do
  resultChars <- many1Till anyChar $ choice (lookAhead <$> parsers)
  pure <<< fromCharArray $ A.fromFoldable resultChars

tagParser :: Parser Doc
tagParser =
  tagParser'
    <?> "A tag must contain at least 1 alphanumeric character"
  where
  tagParser' =
    choice
      [ unescapedVarTagParser
      , escapedVarTagParser
      ]

escapedVarTagParser :: Parser Doc
escapedVarTagParser = EscapedVarTag <$> customTagParser "{{" "}}"

unescapedVarTagParser :: Parser Doc
unescapedVarTagParser = UnescapedVarTag <$> customTagParser "{{{" "}}}"

customTagParser :: String -> String -> Parser String
customTagParser opening closing = between (string opening) (string closing) keyParser

keyParser :: Parser String
keyParser = do
  skipSpaces
  -- firstChar <- anyLetter <?> "A tag must start with a letter"
  firstChar <- anyLetter <|> (anyChar >>= \c -> fail $ "expected char, got " <> show c)
  key <- many1 alphaNum <?> "A tag must contain at least 1 alphanumeric character"
  skipSpaces
  pure $ fromCharArray $ A.fromFoldable $ L.nelCons firstChar key

symbol :: String -> Parser Unit
symbol = string >>> void
