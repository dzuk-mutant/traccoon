module Main exposing (..)

import Browser exposing (Document)
import Html.Styled as Html exposing (Html)
import TimeSheet


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { sheet : TimeSheet.Sheet
    }

type Msg
    = Test

init : () -> ( Model, Cmd Msg)
init _ =
    ( { sheet = TimeSheet.init }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view _ =
    { title = "Traccoon"
    , body = [ ]
    }
