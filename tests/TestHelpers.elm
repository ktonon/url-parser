module TestHelpers exposing (..)


type alias Article =
    { author : String, id : Int }


type BlogRoute
    = Overview
    | Post Int


type Route
    = Search String
    | Blog Int
    | User String
    | Comment String Int


type Route1
    = BlogList (Maybe String)
    | BlogPost Int
