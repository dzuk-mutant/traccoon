module Subtask exposing (Subtask, ID, create, createWithHexColor)


{-| The module that handles Stages - sub-tasks within a project.
-}

import Color exposing (Color)
import Color.Convert

type alias ID = Int

{-| A project may have multiple stages. This allows the
user to keep track of not just the jobs they are doing
but sub-tasks within those jobs.

A stage is defined within a project type.

-}
type alias Subtask =
    { name : String
    , color : Color
    }


{-| Creates a stage with a name string and a color value.
-}
create : String -> Color -> Subtask
create name color =
    { name = name 
    , color = color
    }


{-| Attempts to create a Stage with a Hex Colour input.

If the Hex is invalid, the function will return nothing.
-}
createWithHexColor : String -> String -> Maybe Subtask
createWithHexColor name colorHex =
    case Color.Convert.hexToColor colorHex of
        Err _ ->
            Nothing
        Ok color ->
            Just
            { name = name
            , color = color
            }