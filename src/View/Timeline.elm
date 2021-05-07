module View.Timeline exposing (weekly)

import Calendar
import Html.Styled as Html exposing (Html)
import Sheet exposing (Sheet, Time(..), TimeZone(..))
import Time


weekly : Sheet -> Html msg
weekly sheet =
    let
        noView = Html.div [] []
    in
    case sheet.time of
        NoTime -> noView
        HasTime time ->
            case sheet.zone of
                NoTZ -> noView
                HasTZ zone ->
                    weeklyView time zone sheet


weeklyView : Time.Posix -> Time.Zone -> Sheet -> Html msg
weeklyView time zone sheet =
    let
        timespan = Calendar.weekSpanFromTime zone time
        blocks = Sheet.toProjectsFilteredByTimeframe (Tuple.first timespan) (Tuple.second timespan) sheet

    in
        Html.div [] []