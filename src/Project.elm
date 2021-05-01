module Project exposing (Project, ID, MonetaryValue(..), fromValues)


import Currency
import ProjectType



type alias ID = Int


{-| A Project is a particular work job.
-}
type alias Project =
    { name : String
    , projectType : ProjectType.ID
    , monetaryValue : MonetaryValue
    }

{-| The monetary value of a project (if any.)

There's space here for hourly fees, but that's something I'd be
willing to explore at a later stage, not right now.
-}
type MonetaryValue
    = None
    | FlatFee Currency.Value


{-| Creates a Project from it's base values.
-}
fromValues : String -> ProjectType.ID -> MonetaryValue -> Project
fromValues name projTypeID value =
    { name = name
    , projectType = projTypeID
    , monetaryValue = value
    }