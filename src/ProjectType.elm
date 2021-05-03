module ProjectType exposing
    ( Breakdown(..)
    , ID
    , ProjectType
    , fromValues
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

fromValues : String -> Breakdown -> ProjectType
fromValues name breakdown =
    { name = name
    , breakdown = breakdown
    }
