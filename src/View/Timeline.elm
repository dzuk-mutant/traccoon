module View.Timeline exposing (weekly)

import Html.Styled as Html exposing (Html)
import Sheet exposing (Sheet, Time(..), TimeZone(..))
import Time


weekly : Sheet -> Html msg
weekly sheet =
    let
        time = case sheet.time of
            NoTime -> Nothing
            HasTime t -> Just t
        
        zone = case sheet.zone of
           NoTZ -> Nothing
           HasTZ z -> Just z
    
    in
        if time == Nothing && zone /= Nothing then
            Html.div [] []
        else
            Html.div [] []