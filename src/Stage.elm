module Stage exposing (Stage, ID)


import Color exposing (Color)


type alias ID = Int


{-| A project may have multiple stages. This allows the
user to keep track of not just the jobs they are doing
but sub-tasks within those jobs.

The stage is defined by the project type.

-}
type alias Stage =
    { name : String
    , color : Color
    }
