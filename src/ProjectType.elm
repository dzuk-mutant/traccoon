module ProjectType exposing
    ( Breakdown(..)
    , ID
    , ProjectType
    , fromValues
    , edit
    )

import Array exposing (Array)
import Color exposing (Color)
import Subtask exposing (Subtask)



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- TYPES ---------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

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



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------- CREATION + MANIPULATION -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

{-| Creates a ProjectType from it's base values.
-}
fromValues : String -> Breakdown -> ProjectType
fromValues name breakdown =
    { name = name
    , breakdown = breakdown
    }

{-| Edits a ProjectType's name.

The breakdown of a ProjectType cannot be changed.
-}
edit : String -> ProjectType -> ProjectType
edit newName projType =
    { projType | name = newName }