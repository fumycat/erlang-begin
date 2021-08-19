module Main exposing (..)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Decode.Pipeline exposing (required)


type Msg
    = Fetch (Result Http.Error (List Conn))


type Model
    = Conns (Maybe (List Conn))
    | ErrorMessage String


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
    ( Conns Nothing, loadData )


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
    case model of
        Conns Nothing ->
            Html.text "Loading..."

        Conns (Just l) ->
            Html.div [] (List.intersperse (Html.br [] []) (List.map viewCon l))

        ErrorMessage e ->
            Html.text ("Error: " ++ e)


viewCon : Conn -> Html Msg
viewCon conn =
    Html.p []
        [ Html.text
            (conn.date
                ++ " "
                ++ conn.addr
                ++ " "
                ++ conn.method
                ++ " "
                ++ conn.path
                ++ " "
                ++ String.fromInt conn.status
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Fetch (Ok parts) ->
            ( Conns (Just parts), Cmd.none )

        Fetch (Err error) ->
            ( ErrorMessage (Debug.toString error), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
