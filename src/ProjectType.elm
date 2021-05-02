module ProjectType exposing (Breakdown(..), ID, ProjectType, monolithic, subtasked)

import Array exposing (Array)
import Color exposing (Color)
import Subtask exposing (Subtask)


type alias ID = Int

{-| A Project type notes what kind of project it is.
-}
type alias ProjectType =
    { name : String
    , breakdown : Breakdown
    }


{-| A Project type can be:

- Monolithic (it cannot be broken down into subtasks)
- HasSubtasks (it can be broken down into subtasks)

A Project with a Monolithic ProjectType will still have
Subtask IDs (because of specific technical restrictions of Elm),
it's just that all the IDs will be zero.
-}
type Breakdown
    = Monolithic Color
    | Subtasked (Array Subtask)


{-| Creates a Monolithic ProjectType.
-}
monolithic : String -> Color -> ProjectType
monolithic name color =
    { name = name
    , breakdown = Monolithic color
    }

{-| Creates Subtasked ProjectType.
-}
subtasked : String -> Array Subtask -> ProjectType
subtasked name subtasks =
    { name = name
    , breakdown = Subtasked subtasks
    }
