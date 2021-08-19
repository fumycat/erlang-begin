module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Decode.Pipeline exposing (required)


type Msg
    = Fetch (Result Http.Error (List Conn))
    | Click ViewMode


type Data
    = Conns (Maybe (List Conn))
    | ErrorMessage String


type alias Model =
    { view_mode : ViewMode
    , data : Data
    }


type ViewMode
    = Main
    | Today
    | NotToday
    | Path


type alias Conn =
    { date : String
    , addr : String
    , method : String
    , path : String
    , status : Int
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model Main (Conns Nothing), loadData )


loadData : Cmd Msg
loadData =
    Http.get
        { url = "/get_json"
        , expect = Http.expectJson Fetch (field "data" (Decode.list conDecoder))
        }


conDecoder : Decoder Conn
conDecoder =
    Decode.succeed Conn
        |> required "date" string
        |> required "addr" string
        |> required "method" string
        |> required "path" string
        |> required "status" int


view : Model -> Html Msg
view model =
    case model.data of
        Conns Nothing ->
            Html.text "Loading..."

        Conns (Just l) ->
            layout [ width fill, Background.color (rgb 0.9 0.91 0.7) ]
                (row [ width fill, padding 48 ]
                    [ column
                        [ padding 8, alignTop, width (fillPortion 1), Border.widthEach { bottom = 0, left = 0, top = 0, right = 3 } ]
                        (List.map makeButton
                            [ { onPress = Just (Click Main), label = text "Main" }
                            , { onPress = Just (Click Today), label = text "Today" }
                            , { onPress = Just (Click NotToday), label = text "Not Today" }
                            , { onPress = Just (Click Path), label = text "Path" }
                            ]
                        )
                    , column
                        [ padding 16, width (fillPortion 9), Background.color (rgb 1 1 1) ]
                        (List.map viewCon l)
                    ]
                )

        ErrorMessage e ->
            Html.text ("Error: " ++ e)


makeButton : { onPress : Maybe Msg, label : Element Msg } -> Element Msg
makeButton attr =
    row [ padding 8 ] [ Input.button [] attr ]

viewCon : Conn -> Element Msg
viewCon conn =
    row [ padding 4 ]
        [ text
            (conn.date
                ++ "   "
                ++ conn.addr
                ++ "   "
                ++ conn.method
                ++ "   "
                ++ conn.path
                ++ "   "
                ++ String.fromInt conn.status
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch (Ok parts) ->
            ( Model model.view_mode (Conns (Just parts)), Cmd.none )

        Fetch (Err error) ->
            ( Model model.view_mode (ErrorMessage (Debug.toString error)), Cmd.none )

        Click msg_view_mode ->
            ( { model | view_mode = msg_view_mode }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
