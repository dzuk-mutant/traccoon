module ProjectType exposing (ProjectType, ID)

import Dict exposing (Dict)
import Stage exposing (Stage)


type alias ID = Int


{-| A Project type notes what kind of project it is.
-}
type alias ProjectType =
    { name : String
    , stages : Dict Stage.ID Stage
    }
