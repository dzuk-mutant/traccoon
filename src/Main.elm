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
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


init : () -> ( Model, Cmd Msg )
init _ =
    ( Sheet.init
    , Task.perform AdjustTimeZone Time.here
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( Sheet.updateTime time model, Cmd.none )
        
        AdjustTimeZone zone ->
            ( Sheet.updateTimeZone zone model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- We're doing a high frequency so mis-clicks
    -- don't create accidents in the software.
    Time.every 1 Tick


view : Model -> Document Msg
view _ =
    { title = "Traccoon 🦝⌚"
    , body = []
    }
