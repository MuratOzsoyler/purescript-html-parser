module Text.HTML.Parser.Internal where

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, discard, flip, pure, ($), (*>), (/=), (<$>), (<*), (<<<))
import StringParser (ParseError, Parser, alphaNum, anyChar, between, manyTill, option, runParser, try, whiteSpace)
import StringParser.CodeUnits (char, eof, noneOf, satisfy, skipSpaces, string)
import StringParser.Combinators (fix, many, many1, optionMaybe, sepEndBy)
import Text.HTML.Parser.Combinators (fromChars, isAlphaNumeric, notFollowedBy)
import Text.HTML.Parser.Types (Attribute(..), HTML(..))

parseHTML :: String -> Either ParseError (List HTML)
parseHTML s = flip runParser s $ many parseNode <* eof

singletonElements ∷ Array String
singletonElements = ["meta"]

parseElement :: Parser HTML
parseElement = do
  Tuple name attrs <- parseOpenTag
  children <- many parseNode
  _ <- parseCloseTag name
  pure $ Element name attrs children

parseNode :: Parser HTML
parseNode = fix \_ ->
  try parseElement <|>
  try parseVoidElement <|>
  try parseCommentElement <|>
  try parseDocumentType <|>
  parseTextNode

parseOpenTag :: Parser (Tuple String (List Attribute))
parseOpenTag = do
  notClosedTag
  _ <- char '<'
  name <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  _ <- char '>'
  pure $ Tuple name attrs

parseCloseTag :: String -> Parser Char
parseCloseTag name = do
  _ <- string "</"
  _ <- string name
  char '>'

notClosedTag :: Parser Unit
notClosedTag = notFollowedBy $ string "</"

parseVoidElement :: Parser HTML
parseVoidElement = do
  notClosedTag
  _ <- char '<'
  name <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  _ <- string "/>"
  pure $ VoidElement name attrs

parseTagName :: Parser String
parseTagName = fromChars <$> many1 (satisfy isAlphaNumeric)

parseTextNode :: Parser HTML
parseTextNode = TextNode <<< fromChars <$> many1 (noneOf ['<', '>'])

parseAttributes :: Parser (List Attribute)
parseAttributes = sepEndBy parseAttribute skipSpaces

parseAttribute :: Parser Attribute
parseAttribute = do
  name <- parseAttributeName
  skipSpaces
  maybeEquals <- optionMaybe $ char '='
  value <- case maybeEquals of
    Nothing -> pure ""
    Just _ -> do
        skipSpaces
        parseAttributeValue
  pure $ Attribute name value

parseAttributeName :: Parser String
parseAttributeName = fromChars <$> many1 (noneOf [' ', '"', '\'', '>', '/', '='])

parseAttributeValue :: Parser String
parseAttributeValue = do
  maybeOpenChar <- optionMaybe (char '"' <|> char '\'')
  case maybeOpenChar of
    Nothing -> fromChars <$> many1 (noneOf [' ', '\t', '\n', '\r', '"', '\'', '=', '<', '>', '`', '/'])
    Just openChar -> do
      value <- fromChars <$> many (noneOf [openChar])
      _ <- char openChar
      pure value

parseCommentElement :: Parser HTML
parseCommentElement = do
  _ <- string "<!--"
  CommentNode <<< fromChars <$> contents
  where
    contents = manyTill anyChar end 
    end = string "-->"

parseDocumentType :: Parser HTML
parseDocumentType = do
  _ <- start *> many whiteSpace
  name <- option "" $ (fromChars <$> (many1 alphaNum)) <* skipSpaces
  publicId <- option "" do
    _ <- public *> skipSpaces 
    between strSep strSep str <* skipSpaces   
  systemId <- option "" $ between strSep strSep str <* skipSpaces
  _ <- end
  pure $ DocumentType {name, publicId, systemId}
  where
    start = try (string "<!DOCTYPE") <|> try (string "<!doctype")
    public = try (string "PUBLIC") <|> try (string "public")
    str = fromChars <$> many (satisfy (_ /= '"'))
    strSep = char '"'
    end = char '>'