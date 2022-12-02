module Test.Main where

import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (fromFoldable)
import Effect (Effect)
import Prelude (Unit, discard, show, ($), (/=), (<>))
import StringParser (printParserError)
import Test.Utils (fail, runTest, success)
import Text.HTML.Parser (Attribute(..), HTML, parseHTML)
import Text.HTML.Parser.Array (commentNode, documentType, element, textNode, voidElement)

main :: Effect Unit
main = runTest do
  assertParse "<html></html>"
    [ element "html" [] [] ]

  assertParse "<html><body></body><head></head></html>"
    [ element "html" [] $
        [ element "body" [] []
        , element "head" [] []
        ]
    ]

  assertParse "<br/>"
    [ voidElement "br" [] ]

  assertParse "<p><br/></p>"
    [ element "p" [] [voidElement "br" []] ]

  assertParse "<div></div><span></span>"
    [ element "div" [] []
    , element "span" [] []
    ]

  assertParse "<div>hello<em>big</em>world!</div>"
    [ element "div" []
      [ textNode "hello"
      , element "em" [] [textNode "big"]
      , textNode "world!"
      ]
    ]

  assertParse """<div id="foo" class="bar"></div>"""
    [ element "div"
      [ Attribute "id" "foo"
      , Attribute "class" "bar"
      ] []
    ]

  assertParse """<div id = 'foo' disabled class =bar></div>"""
    [ element "div"
      [ Attribute "id" "foo"
      , Attribute "disabled" ""
      , Attribute "class" """bar"""
      ] []
    ]

  assertParse """<div id="foo"><img src="puppies.gif"/></div>"""
    [ element "div" [Attribute "id" "foo"]
      [ voidElement "img" [Attribute "src" "puppies.gif"] ]
    ]

    -- See if whitespace after attributes affect parsing...
  assertParse """<br class="solid" />"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if unquoted attribute value is parsed properly...
  assertParse """<br class=solid/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if spaces around the equals sign affect attribute parsing...
  assertParse """<br class = "solid"/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

    -- See if spaces around the equals sign affect attribute parsing if argument value is not
    --        quoted...

  assertParse """<br class = solid/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    ]

  -- See if comment can be parsed

  assertParse """<!-- abcdef-->"""
    [ commentNode " abcdef"]

  -- See if comment can be parsed between elements

  assertParse """<br class="solid"/><!-- abcd --><i/>"""
    [ voidElement "br" [Attribute "class" "solid"]
    , commentNode " abcd "
    , voidElement "i" []
    ]

  -- See if it can parse empty DocumentType

  assertParse """<!DOCTYPE>"""
    [ documentType "" "" ""]

  -- See if it can parse DocumentType with name only

  assertParse """<!DOCTYPE html >"""
    [ documentType "html" "" ""]

  -- See if it can parse DocumentType with name and publicId

  assertParse """<!DOCTYPE html public "-//W3C//DTD HTML 4.0 Transitional//EN" >"""
    [ documentType "html" "-//W3C//DTD HTML 4.0 Transitional//EN" ""]

  -- See if it can parse DocumentType with name and publicId and systemId

  assertParse """<!DOCTYPE html public "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">"""
    [ documentType "html" "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd"]

assertParse
  :: forall f. (Foldable f)
  => String -> f HTML -> Effect Unit
assertParse input expected = case parseHTML input of
  Left err ->
    fail $ "Parse error for " <> show input <> "\n  Error: " <> printParserError err
  Right actual | actual /= expected' ->
    fail $ "Expected: " <> show expected' <> "\n  Actual: " <> show actual
  _ -> success input
  where
  expected' = fromFoldable expected
