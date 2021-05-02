module Project exposing (ID, MonetaryValue(..), Project, addBlock, fromValues, moneyPerHour)

{-| A project is a particular job that has a clear beginning and end.
-}

import Block exposing (Block)
import Currency
import ProjectType
import Time



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
------------------------------- TYPES -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


type alias ID =
    Int


{-| A Project is a particular work job.

Blocks are the periods of time worked on this particular project.
They are listed chronologically from the earliest to the latest.
There should never be overlapping blocks.

-}
type alias Project =
    { name : String
    , projectType : ProjectType.ID
    , monetaryValue : MonetaryValue
    , blocks : List Block
    }


{-| The monetary value of a project (if any.)
-}
type MonetaryValue
    = None
    | FlatFee Currency.Value
    | Hourly Currency.Value



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- FUNCTIONS -----------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Creates a Project from it's base values.
-}
fromValues : String -> ProjectType.ID -> MonetaryValue -> Project
fromValues name projTypeID value =
    { name = name
    , projectType = projTypeID
    , monetaryValue = value
    , blocks = [] -- Projects start with no blocks
    }


{-| Adds a block to a project.
-}
addBlock : Block -> Project -> Project
addBlock newBlock proj =
    { proj | blocks = List.append proj.blocks [ newBlock ] }


{-| Takes a Project and returns the amount of money earned
per hour on that Project.

If the Project didn't have a monetary value to begin with,
then this function returns Nothing.

-}
moneyPerHour : Project -> Maybe Currency.Value
moneyPerHour proj =
    case proj.monetaryValue of
        None ->
            Nothing

        FlatFee val ->
            let
                hourlyValue =
                    proj.blocks
                        |> List.map (\b -> Time.posixToMillis b.end - Time.posixToMillis b.start)
                        |> List.foldl (+) 0
                        -- add all the milliseconds
                        |> (\v -> toFloat v / 3600000)
                        -- turn into hours
                        |> (\v -> val.amount / v)
            in
            -- return the monthly val with the same currency.
            Just { val | amount = hourlyValue }

        Hourly val ->
            Just val
