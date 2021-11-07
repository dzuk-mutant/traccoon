module View.Timeline exposing (weekly)

import Calendar
import Externalized
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
        week = Calendar.weekSpanFromTime zone time
        blocks =
            sheet
            |> Sheet.toProjectsFilteredByTimeframe week
            |> Externalized.blocksFromProjectDict
            |> Externalized.sortBlocks

    in
        Html.div [] []





