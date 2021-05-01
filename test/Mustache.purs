module Test.Mustache where

import Prelude
import Data.Argonaut ((:=))
import Data.Argonaut as J
import Data.Either (Either(..))
import Data.Traversable (for_)
import Foreign.Object as O
import Mustache (render)
import Test.Spec (Spec, describe, it, pending, pending', focus)
import Test.Spec.Assertions (shouldContain, shouldEqual)
import Text.Parsing.StringParser (ParseError(..))

main :: Spec Unit
main = do
  describe "Mustache module" do
    describe "render function" do
      testSimpleTags
      testParentContext
      testSections
      testInvertedSections

noRenderSection :: Spec Unit
noRenderSection =
  for_ emptyValues \emptyVal -> do
    it ("should not render a section when the json value is " <> J.stringify emptyVal) do
      let
        template =
          """
          {{#norender}}
            section body
          {{/norender}}
          """

        expected =
          """
          """

        view = J.fromObject $ O.fromFoldable [ "norender" := emptyVal ]
      render template view `shouldContain` expected
  where
  emptyValues =
    [ J.encodeJson false
    , J.encodeJson 0
    , J.encodeJson ""
    , J.encodeJson ([] :: Array String)
    ]

renderInvertedSections :: Spec Unit
renderInvertedSections =
  for_ invertedValues \inverted -> do
    it ("should render an inverted section when the json is " <> (J.stringify $ J.encodeJson inverted)) do
      let
        template =
          """
          {{^json}}
          no json
          {{/json}}
          """

        expected =
          """
          no json
          """

        view = J.fromObject inverted
      render template view `shouldContain` expected
  where
  invertedValues =
    [ [ "notjson" := 1 ]
    , [ "json" := "" ]
    , [ "json" := false ]
    , [ "json" := 0 ]
    , [ "json" := ([] :: Array String) ]
    ]
      <#> O.fromFoldable

testSimpleTags :: Spec Unit
testSimpleTags =
  describe "simle tags" do
    it "should output a template without a view without modification" do
      let
        template = "Tags {{first}}"

        view = J.fromObject $ O.empty
      render template view `shouldContain` template
    it "should output a template without tags without modification" do
      let
        template = "No tags"

        view = J.fromObject $ O.empty
      render template view `shouldContain` template
    it "should output a rendered template with only a tag" do
      let
        template = "{{first}}"

        view = J.fromObject $ O.fromFoldable [ "first" := "work" ]
      render template view `shouldContain` "work"
    it "should output a rendered template with a simple tag" do
      let
        template = "Tags {{first}}"

        view = J.fromObject $ O.fromFoldable [ "first" := "work" ]
      render template view `shouldContain` "Tags work"
    it "should output a rendered template with a simple tag and more plain text" do
      let
        template = "Tags {{first}} and more"

        view = J.fromObject $ O.fromFoldable [ "first" := "work" ]
      render template view `shouldContain` "Tags work and more"
    it "should output a rendered template with a mulitple tags multiple plain text" do
      let
        template = "Tags {{first}} and {{second}} tags work as well"

        view = J.fromObject $ O.fromFoldable [ "first" := "work", "second" := 2 ]
      render template view `shouldContain` "Tags work and 2 tags work as well"
    it "should render every single type of json" do
      let
        template = "{{float}} {{int}} {{bool}} {{string}}"

        view = J.fromObject $ O.fromFoldable [ "float" := 3.14, "int" := 2, "bool" := true, "string" := "text" ]
      render template view `shouldContain` "3.14 2 true text"
    it "should render a tag with an array of numbers" do
      let
        template = "{{numbers}}"

        view = J.fromObject $ O.fromFoldable [ "numbers" := [ 1, 2, 3 ] ]
      render template view `shouldContain` "1,2,3"
    it "should render a tag with spaces" do
      let
        template = "{{  tag    }}"

        view = J.fromObject $ O.fromFoldable [ "tag" := "tag" ]
      render template view `shouldContain` "tag"
    it "should render a tag with an all caps key" do
      let
        template = "{{TAG}}"

        view = J.fromObject $ O.fromFoldable [ "TAG" := "tag" ]
      render template view `shouldContain` "tag"
    it "should render an unescaped tag" do
      let
        template = "{{{tag}}}"

        view = J.fromObject $ O.fromFoldable [ "tag" := "tag" ]
      render template view `shouldContain` "tag"
    it "should render an array tag and a string tag" do
      let
        template =
          """
          array: {{arr}}{{^arr}}{{defaultArr}}{{/arr}}
          string: {{str}}
          """

        expected =
          """
          array: 1,2,3
          string: string
          """

        view = J.fromObject $ O.fromFoldable [ "arr" := [ 1, 2, 3 ], "str" := "string", "defaultArr" := "no arr" ]
      render template view `shouldContain` expected
    pending' "should render a tag with escaped html" do
      let
        template = "{{tag}}"

        expected = "escaped"

        view = J.fromObject $ O.fromFoldable [ "tag" := "<b>escaped</b>" ]
      render template view `shouldContain` expected
    it "should not render an empty tag" do
      let
        template = "{{}}"

        view = J.fromObject $ O.empty
      render template view `shouldEqual` (Left $ "A tag must start with a letter")
    it "should not render a tag with a key that starts with a number" do
      let
        template = "{{1tag}}"

        view = J.fromObject $ O.empty
      render template view `shouldEqual` (Left $ "A tag must start with a letter")

testSections :: Spec Unit
testSections =
  describe "sections" do
    noRenderSection
    it "should not render a section when the json is a 0 value" do
      let
        template =
          """
          {{#section}}
          section body {{section}}
          {{/section}}
          """

        expected =
          """
          """

        view = J.fromObject $ O.fromFoldable [ "section" := 0 ]
      render template view `shouldContain` expected
    it "should render a section when the json is a true value" do
      let
        template =
          """
          {{#section}}
          section body
          {{/section}}
          """

        expected =
          """
          section body
          """

        view = J.fromObject $ O.fromFoldable [ "section" := true ]
      render template view `shouldContain` expected
    it "should render a section when the json is an object" do
      let
        template =
          """
          {{#user}}
          first name: {{firstName}}
          last name: {{lastName}}
          {{/user}}
          """

        expected =
          """
          first name: John
          last name: Doe
          """

        user = O.fromFoldable [ "firstName" := "John", "lastName" := "Doe" ]

        view = J.fromObject $ O.fromFoldable [ "user" := user ]
      render template view `shouldContain` expected
    it "should render an empty section when the json is truthy" do
      let
        template = "{{#user}}{{/user}}"

        expected = ""

        view = J.fromObject $ O.fromFoldable [ "user" := (O.empty :: O.Object String) ]
      render template view `shouldContain` expected
    it "should render a nested section when the json is an object" do
      let
        template =
          """
          {{#user}}
          name:
          {{#name}}
          first: {{first}}
          last: {{last}}
          {{/name}}
          age: {{age}}
          {{/user}}
          """

        expected =
          """
          name:
          first: John
          last: Doe
          age: 30
          """

        name = O.fromFoldable [ "first" := "John", "last" := "Doe" ]

        user = O.fromFoldable [ "name" := name, "age" := 30 ]

        view = J.fromObject $ O.fromFoldable [ "user" := user ]
      render template view `shouldContain` expected
    it "should repeatedly render a section when the json is a list of objects" do
      let
        template =
          """
          {{#users}}
          first name: {{firstName}}
          last name: {{lastName}}
          {{/users}}
          """

        expected =
          """
          first name: John
          last name: Doe
          first name: Jane
          last name: Doe
          """

        users =
          [ O.fromFoldable [ "firstName" := "John", "lastName" := "Doe" ]
          , O.fromFoldable [ "firstName" := "Jane", "lastName" := "Doe" ]
          ]

        view = J.fromObject $ O.fromFoldable [ "users" := users ]
      render template view `shouldContain` expected

testInvertedSections :: Spec Unit
testInvertedSections =
  describe "inverted sections" do
    it "should not render an inverted section when the json is an object" do
      let
        template =
          """
          {{^json}}
          no json
          {{/json}}
          """

        expected =
          """
          """

        view = J.fromObject $ O.fromFoldable [ "json" := (O.empty :: O.Object Unit) ]
      render template view `shouldContain` expected
    it "should correctly render an inverted section with newlines" do
      let
        template = "{{^json}}no json\n\n\n{{/json}}\nrest"

        expected = "no json\n\n\nrest"

        view = J.fromObject $ O.fromFoldable [ "test" := (O.empty :: O.Object Unit) ]
      render template view `shouldContain` expected
    it "should correctly render an inverted section with newlines and spaces" do
      let
        template = "{{^json}}\n  \nno json\n\n\n{{/json}}\nrest"

        expected = "  \nno json\n\n\nrest"

        view = J.fromObject $ O.fromFoldable [ "test" := (O.empty :: O.Object Unit) ]
      render template view `shouldContain` expected
    renderInvertedSections

testParentContext :: Spec Unit
testParentContext =
  describe "parent context" do
    it "should refer to the parent context if a key cannot be found in the current context" do
      let
        template =
          """
          {{#user}}
          {{name}} has {{parent}}
          {{/user}}
          """

        expected =
          """
          John has parent
          """

        user = J.fromObject $ O.fromFoldable [ "name" := "John" ]

        view = J.fromObject $ O.fromFoldable [ "user" := user, "parent" := "parent" ]
      render template view `shouldContain` expected
    it "should work with nested sections" do
      let
        template =
          """
          {{#user}}
          user name: {{name}}
          {{#parent}}
          parent name: {{name}}
          {{/parent}}
          {{/user}}
          """

        expected =
          """
          user name: user
          parent name: parent
          """

        user = J.fromObject $ O.fromFoldable [ "name" := "user" ]

        parent = J.fromObject $ O.fromFoldable [ "name" := "parent" ]

        view = J.fromObject $ O.fromFoldable [ "user" := user, "parent" := parent ]
      render template view `shouldContain` expected
