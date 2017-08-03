module UrlParser exposing
  ( Parser, string, int, s
  , (</>), map, oneOf, top, custom
  , QueryParser, (<?>), stringParam, intParam, customParam
  , parseString
  , parsePath, parseHash
  )

{-|

Examples below assume the following imports:

    import TestHelpers exposing (..)

# Primitives
@docs Parser, string, int, s

# Path Parses
@docs (</>), map, oneOf, top, custom

# Query Parameter Parsers
@docs QueryParser, (<?>), stringParam, intParam, customParam

# Run a Parser
@docs parseString

# Navigation Compatibility

These parsers are a convenience for working with
[Navigation.Location](http://package.elm-lang.org/packages/elm-lang/navigation/2.1.0/Navigation#Location)

@docs parsePath, parseHash
-}

import Dict exposing (Dict)
import Http


-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
-}
type Parser a b =
  Parser (State a -> List (State b))


type alias State value =
  { visited : List String
  , unvisited : List String
  , params : Dict String String
  , value : value
  }



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.

    parseString string "/alice/"
    --> Just "alice"

    parseString string "/bob"
    --> Just "bob"

    parseString string "/42/"
    --> Just "42"

    parseString string "/"
    --> Nothing
-}
string : Parser (String -> a) a
string =
  custom "STRING" <|
    \segment ->
      if String.isEmpty segment then
        Err "string does not match empty segment"
      else
        Ok segment


{-| Parse a segment of the path as an `Int`.

    parseString int "/alice/"
    --> Nothing

    parseString int "/bob"
    --> Nothing

    parseString int "/42/"
    --> Just 42
-}
int : Parser (Int -> a) a
int =
  custom "NUMBER" String.toInt


{-| Parse a segment of the path if it matches a given string.

    (parseString (s "blog") "/blog/") /= Nothing
    --> True

    (parseString (s "blog") "/glob/") == Nothing
    --> True
-}
s : String -> Parser a a
s str =
  Parser <| \{ visited, unvisited, params, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        if next == str then
          [ State (next :: visited) rest params value ]

        else
          []


{-| Create a custom path segment parser. Here is how it is used to define the
`int` parser:

    int =
      custom "NUMBER" String.toInt

You can use it to define something like “only CSS files” like this:

    let
        css =
            custom "CSS_FILE" <|
                \segment ->
                    if String.endsWith ".css" segment then
                        Ok segment
                    else
                        Err "Does not end with .css"
    in
        ( parseString css "/README.md"
        , parseString css "/style.css"
        )
    --> ( Nothing
    --> , Just "style.css"
    --> )
-}
custom : String -> (String -> Result String a) -> Parser (a -> b) b
custom tipe stringToSomething =
  Parser <| \{ visited, unvisited, params, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        case stringToSomething next of
          Ok nextValue ->
            [ State (next :: visited) rest params (value nextValue) ]

          Err msg ->
            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.

    let
        blog = s "blog" </> int
    in
        ( parseString blog "/blog/35/"
        , parseString blog "/blog/42"
        , parseString blog "/blog/"
        , parseString blog "/42/"
        )
    --> ( Just 35
    --> , Just 42
    --> , Nothing
    --> , Nothing
    --> )

    let
        search = s "search" </> string
    in
        ( parseString search "/search/cats/"
        , parseString search "/search/frog"
        , parseString search "/search/"
        , parseString search "/cats/"
        )
    --> ( Just "cats"
    --> , Just "frog"
    --> , Nothing
    --> , Nothing
    --> )
-}
(</>) : Parser a b -> Parser b c -> Parser a c
(</>) (Parser parseBefore) (Parser parseAfter) =
  Parser <| \state ->
    List.concatMap parseAfter (parseBefore state)


infixr 7 </>


{-| Transform a path parser.

    type alias Article =
        { author : String, id : Int }

    let
        rawArticle = s "user" </> string </> s "articles" </> int
        article = map Article rawArticle
    in
        ( parseString article "/user/bob/articles/42"
        , parseString article "/user/tom/articles/35"
        , parseString article "/user/sam/"
        )
    --> ( Just { author = "bob", id = 42 }
    --> , Just { author = "tom", id = 35 }
    --> , Nothing
    --> )
-}
map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parse) =
  Parser <| \{ visited, unvisited, params, value } ->
    List.map (mapHelp value) <| parse <|
      { visited = visited
      , unvisited = unvisited
      , params = params
      , value = subValue
      }


mapHelp : (a -> b) -> State a -> State b
mapHelp func {visited, unvisited, params, value} =
  { visited = visited
  , unvisited = unvisited
  , params = params
  , value = func value
  }


{-| Try a bunch of different path parsers.

    type Route
        = Search String
        | Blog Int
        | User String
        | Comment String Int

    let
        route =
            oneOf
                [ map Search (s "search" </> string)
                , map Blog (s "blog" </> int)
                , map User (s "user" </> string)
                , map Comment (s "user" </> string </> s "comments" </> int)
                ]
    in
        ( ( parseString route "/search/cats"
          , parseString route "/search/"
          )
        , ( parseString route "/blog/42"
          , parseString route "/blog/cats"
          )
        , ( parseString route "/user/sam/"
          )
        , ( parseString route "/user/bob/comments/42"
          , parseString route "/user/tom/comments/35"
          , parseString route "/user/"
          )
        )
    --> ( ( Just (Search "cats")
    -->   , Nothing
    -->   )
    --> , ( Just (Blog 42)
    -->   , Nothing
    -->   )
    --> , ( Just (User "sam")
    -->   )
    --> , ( Just (Comment "bob" 42)
    -->   , Just (Comment "tom" 35)
    -->   , Nothing
    -->   )
    --> )
-}
oneOf : List (Parser a b) -> Parser a b
oneOf parsers =
  Parser <| \state ->
    List.concatMap (\(Parser parser) -> parser state) parsers


{-| A parser that does not consume any path segments.

    type BlogRoute = Overview | Post Int

    let
        blogRoute =
            oneOf
                [ map Overview top
                , map Post  (s "post" </> int)
                ]
        blog =
            s "blog" </> blogRoute
    in
        ( parseString blog "/blog/"
        , parseString blog "/blog/post/42"
        )
    --> ( Just Overview
    --> , Just (Post 42)
    --> )
-}
top : Parser a a
top =
  Parser <| \state -> [state]



-- QUERY PARAMETERS


{-| Turn query parameters like `?name=tom&age=42` into nice Elm data.
-}
type QueryParser a b =
  QueryParser (State a -> List (State b))


{-| Parse some query parameters.

    type Route1 = BlogList (Maybe String) | BlogPost Int

    let
        route =
            oneOf
                [ map BlogList (s "blog" <?> stringParam "search")
                , map BlogPost (s "blog" </> int)
                ]
    in
        ( parseString route "/blog/"
        , parseString route "/blog/?search=cats"
        , parseString route "/blog/42"
        )
    --> ( Just (BlogList Nothing)
    --> , Just (BlogList (Just "cats"))
    --> , Just (BlogPost 42)
    --> )
-}
(<?>) : Parser a b -> QueryParser b c -> Parser a c
(<?>) (Parser parser) (QueryParser queryParser) =
  Parser <| \state ->
    List.concatMap queryParser (parser state)


infixl 8 <?>


{-| Parse a query parameter as a `String`.

    let
        blog = s "blog" <?> stringParam "search"
    in
        ( parseString blog "/blog/"
        , parseString blog "/blog/?search=cats"
        )
    --> ( Just Nothing
    --> , Just (Just "cats")
    --> )
-}
stringParam : String -> QueryParser (Maybe String -> a) a
stringParam name =
  customParam name identity


{-| Parse a query parameter as an `Int`. Maybe you want to show paginated
search results. You could have a `start` query parameter to say which result
should appear first.

    let
        results = s "results" <?> intParam "start"
    in
        ( parseString results "/results"
        , parseString results "/results?start=10"
        )
    --> ( Just Nothing
    --> , Just (Just 10)
    --> )
-}
intParam : String -> QueryParser (Maybe Int -> a) a
intParam name =
  customParam name intParamHelp


intParamHelp : Maybe String -> Maybe Int
intParamHelp maybeValue =
  case maybeValue of
    Nothing ->
      Nothing

    Just value ->
      Result.toMaybe (String.toInt value)


{-| Create a custom query parser. You could create parsers like these:

    jsonParam : String -> Decoder a -> QueryParser (Maybe a -> b) b
    enumParam : String -> Dict String a -> QueryParser (Maybe a -> b) b

It may be worthwhile to have these in this library directly. If you need
either one in practice, please open an issue [here][] describing your exact
scenario. We can use that data to decide if they should be added.

[here]: https://github.com/evancz/url-parser/issues
-}
customParam : String -> (Maybe String -> a) -> QueryParser (a -> b) b
customParam key func =
  QueryParser <| \{ visited, unvisited, params, value } ->
    [ State visited unvisited params (value (func (Dict.get key params))) ]



-- RUN A PARSER


{-| Parse based on the provided string.

Any of the following formats are accepted

    "path/without/leading/slash"
    "/path/with/leading/slash"
    "/path/with/trailing/slash/"
    "#hash/path"
    "#/hash/path"
    "/path?with=query&keys=andValues"
-}
parseString : Parser (a -> a) a -> String -> Maybe a
parseString parser pathWithQuery =
  splitPathAndQuery pathWithQuery
    |> Maybe.andThen
      (\( pathString, queryParams ) ->
        parse parser pathString queryParams
      )


-- Same as Navigation.Location.
--
-- Reproduced here to avoid a dependency on that library, which does not function
-- outside of the browser.

type alias Location =
  { href : String
  , host : String
  , hostname : String
  , protocol : String
  , origin : String
  , port_ : String
  , pathname : String
  , search : String
  , hash : String
  , username : String
  , password : String
  }


{-| Parse based on `location.pathname` and `location.search`. This parser
ignores the hash entirely.
-}
parsePath : Parser (a -> a) a -> Location -> Maybe a
parsePath parser location =
  parse parser location.pathname (parseParams location.search)


{-| Parse based on `location.hash` and `location.search`. This parser
ignores the normal path entirely.
-}
parseHash : Parser (a -> a) a -> Location -> Maybe a
parseHash parser location =
  parse parser (String.dropLeft 1 location.hash) (parseParams location.search)



-- PARSER HELPERS


parse : Parser (a -> a) a -> String -> Dict String String -> Maybe a
parse (Parser parser) url params =
  parseHelp <| parser <|
    { visited = []
    , unvisited = splitUrl url
    , params = params
    , value = identity
    }


parseHelp : List (State a) -> Maybe a
parseHelp states =
  case states of
    [] ->
      Nothing

    state :: rest ->
      case state.unvisited of
        [] ->
          Just state.value

        [""] ->
          Just state.value

        _ ->
          parseHelp rest


splitPathAndQuery : String -> Maybe ( String, Dict String String )
splitPathAndQuery rawPath =
  case String.split "?" rawPath of
    [] ->
      Just ( "", Dict.empty )

    [ path ] ->
      Just ( path, Dict.empty )

    [ path, rawQuery ] ->
      Just ( path, parseParams rawQuery )

    _ ->
      Nothing


splitUrl : String -> List String
splitUrl url =
  case url |> stripLeft "#" |> String.split "/" of
    "" :: segments ->
      segments

    segments ->
      segments


parseParams : String -> Dict String String
parseParams queryString =
  queryString
    |> stripLeft "?"
    |> String.split "&"
    |> List.filterMap toKeyValuePair
    |> Dict.fromList


stripLeft : String -> String -> String
stripLeft pattern string =
    if string |> String.startsWith pattern then
        String.dropLeft (String.length pattern) string
    else
        string

toKeyValuePair : String -> Maybe (String, String)
toKeyValuePair segment =
  case String.split "=" segment of
    [key, value] ->
      Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

    _ ->
      Nothing
