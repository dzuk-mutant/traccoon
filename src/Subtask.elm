module Subtask exposing
    ( ID
    , Subtask
    , edit
    , fromStrAndHexColor
    , fromValues
    )

{-| The module that handles Subtasks within a project.
-}

import Color exposing (Color)
import Color.Convert


type alias ID =
    Int


{-| A project may have multiple Subtasks. This allows the
user to keep track of not just the jobs they are doing
but sub-tasks within those jobs.

A stage is defined within a project type.

-}
type alias Subtask =
    { name : String
    , color : Color
    }


{-| Creates a Subtask with a name string and a color value.
-}
fromValues : String -> Color -> Subtask
fromValues name color =
    { name = name
    , color = color
    }


{-| Attempts to create a Subtask with a Hex Colour input.

If the Hex is invalid, the function will return nothing.

-}
fromStrAndHexColor : String -> String -> Maybe Subtask
fromStrAndHexColor name colorHex =
    case Color.Convert.hexToColor colorHex of
        Err _ ->
            Nothing

        Ok color ->
            Just
                { name = name
                , color = color
                }


{-| Changes a Subtask's name and color.
-}
edit : String -> Color -> Subtask -> Subtask
edit name color subtask =
    { subtask
        | name = name
        , color = color
    }
