module ProjectType exposing (ProjectType, ID, create)

import Dict exposing (Dict)
import Stage exposing (Stage)


type alias ID = Int


{-| A Project type notes what kind of project it is.
-}
type alias ProjectType =
    { name : String
    , stages : Maybe (Dict Stage.ID Stage)
    }

create : String -> Maybe (Dict Stage.ID Stage) -> ProjectType
create name stages =
    { name = name
    , stages = stages
    }
