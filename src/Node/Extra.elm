module Node.Extra exposing (equals, values)

import Elm.Syntax.Node as Node exposing (Node)
import Ra


values : List (Node a) -> List a
values =
    List.map Node.value


equals : Node a -> Node a -> Bool
equals =
    Ra.fnContraMap2 Node.value Ra.equals
