module Tests exposing (..)

import Dict
import UrlParser exposing (..)
import Test exposing (..)
import Expect


-- TESTS


all : Test
all =
    describe "UrlParser"
        [ describe "Basic Parsing" testParsing
        ]


testParsing : List Test
testParsing =
    [ parserTest "Home" "" HomeRoute
    , parserTest "About" "about" AboutRoute
    , parserTest "Token" "token/abc" (TokenRoute "abc")
    , parserTest "Users" "users" (UsersRoutes UsersRoute)
    , parserTest "User" "users/2" (UsersRoutes (UserRoute 2))
    , parserTest "Edit" "users/2/edit" (UsersRoutes (UserEditRoute 2))
    ]


parserTest : String -> String -> MainRoute -> Test
parserTest name path expectedRoute =
    describe name
        [ test (name ++ " in path") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parse routeParser ("/" ++ path) Dict.empty)
        , test (name ++ " in hash") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parse routeParser ("#/" ++ path) Dict.empty)
        , test (name ++ "in hash without leading slash") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parse routeParser ("#" ++ path) Dict.empty)
        ]



-- ROUTES


type alias UserId =
    Int


type UserRoute
    = UsersRoute
    | UserRoute UserId
    | UserEditRoute UserId


type MainRoute
    = HomeRoute
    | AboutRoute
    | TokenRoute String
    | UsersRoutes UserRoute
    | NotFoundRoute



-- PARSERS


routeParser : Parser (MainRoute -> c) c
routeParser =
    oneOf mainMatchers


usersMatchers : List (Parser (UserRoute -> c) c)
usersMatchers =
    [ map UserEditRoute (int </> s "edit")
    , map UserRoute (int)
    , map UsersRoute top
    ]


mainMatchers : List (Parser (MainRoute -> c) c)
mainMatchers =
    [ map HomeRoute top
    , map AboutRoute (s "about")
    , map TokenRoute (s "token" </> string)
    , map UsersRoutes (s "users" </> (oneOf usersMatchers))
    ]
