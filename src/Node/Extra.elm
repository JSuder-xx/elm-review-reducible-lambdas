module Node.Extra exposing (values)

import Elm.Syntax.Node as Node exposing (Node)


values : List (Node a) -> List a
values =
    List.map Node.value
