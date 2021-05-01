module Project exposing (Project, ID, create)

import ProjectType exposing (ProjectType)



type alias ID = Int


{-| A Project is a particular work job.
-}
type alias Project =
    { name : String
    , projectType : ProjectType
    }


create : String -> ProjectType -> Project
create name projectType =
    { name = name
    , projectType = projectType
    }