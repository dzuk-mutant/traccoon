module Main exposing (..)

-- import Html.Styled as Html exposing (Html)

import Browser exposing (Document)
import Sheet exposing (Sheet)
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Sheet


type Msg
    = Ticked Time.Posix
    | AdjustedTimeZone Time.Zone


init : () -> ( Model, Cmd Msg )
init _ =
    ( Sheet.init
    , Task.perform AdjustedTimeZone Time.here
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ticked time ->
            ( Sheet.updateTime time model, Cmd.none )

        AdjustedTimeZone zone ->
            ( Sheet.updateTimeZone zone model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1 Ticked


view : Model -> Document Msg
view _ =
    { title = "Traccoon 🦝⌚"
    , body = []
    }
