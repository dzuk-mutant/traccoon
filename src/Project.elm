module Project exposing (Project, ID, MonetaryValue(..), create)

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
-}
type MonetaryValue
    = None
    | FlatFee Float
    | HourlyRate Float


create : String -> ProjectType.ID -> MonetaryValue -> Project
create name projTypeID value =
    { name = name
    , projectType = projTypeID
    , monetaryValue = value
    }