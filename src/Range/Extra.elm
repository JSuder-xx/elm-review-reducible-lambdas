module Range.Extra exposing (expandToLeft, expandToRight, toTheLeft, toTheRight)

import Elm.Syntax.Range as Range exposing (Range)


expandToRight : Int -> Range -> Range
expandToRight n { start, end } =
    { start = start
    , end =
        { row = end.row
        , column = end.column + n
        }
    }


expandToLeft : Int -> Range -> Range
expandToLeft n { start, end } =
    { start =
        { row = start.row
        , column = start.column - n
        }
    , end = end
    }


toTheLeft : { delta : Int, count : Int } -> Range -> Range
toTheLeft { delta, count } { start, end } =
    let
        endColumn =
            start.column - delta
    in
    { end =
        { row = end.row
        , column = endColumn
        }
    , start =
        { row = start.row
        , column = endColumn - count
        }
    }


toTheRight : { delta : Int, count : Int } -> Range -> Range
toTheRight { delta, count } { start, end } =
    let
        startColumn =
            end.column + delta
    in
    { start =
        { row = start.row
        , column = startColumn
        }
    , end =
        { row = end.row
        , column = startColumn + count
        }
    }
