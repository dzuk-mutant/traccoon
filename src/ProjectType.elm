module ProjectType exposing (ProjectType, ID, Breakdown(..), createMonolithic, createSubtasked)

import Array exposing (Array)
import Color exposing (Color)
import Subtask exposing (Subtask)


type alias ID = Int


{-| A Project type notes what kind of project it is.

A project may or may not have stages.
-}
type alias ProjectType =
    { name : String
    , breakdown : Breakdown
    }

{-| A Project type can be:
    - Monolithic (it cannot be broken down into subtasks)
    - HasSubtasks (it can be broken down into subtasks)
-}
type Breakdown
    = Monolithic Color
    | Subtasked (Array Subtask)

createMonolithic : String -> Color -> ProjectType
createMonolithic name color =
    { name = name
    , breakdown = Monolithic color
    }

createSubtasked : String -> Array Subtask -> ProjectType
createSubtasked name subtasks =
    { name = name
    , breakdown = Subtasked subtasks
    }