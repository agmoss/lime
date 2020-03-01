module Main exposing (main)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Http exposing (expectJson, request,header)
import Json.Decode as D exposing (Decoder, field, string)
import RemoteData exposing (RemoteData(..), WebData)

--- RYME records & decoders

type alias Words =
    { word : String
    , rhymes : Rhymes
    }

type alias Rhymes =
    { all : List String
    }


stringArrayDecoder : D.Decoder (List String)
stringArrayDecoder =
    D.list D.string

decodeWords : Decoder Words
decodeWords =
    D.map2 Words
        (D.field "word"  D.string)
        (D.field "rhymes"  decodeRhymes)

decodeRhymes : Decoder Rhymes
decodeRhymes = 
    D.map Rhymes 
        (D.field "all" stringArrayDecoder )
        



---- MODEL ----


type alias Model =
    {rhymesData: WebData Words}


init : ( Model )
init =
    ( {rhymesData = Loading} )

type Msg
    = RhymesDataResponse (WebData Words)


fetchRhymesData : Cmd Msg
fetchRhymesData =
    Http.request 
        { method = "GET"
        , headers = [ header "x-rapidapi-host" "wordsapiv1.p.rapidapi.com", header "x-rapidapi-key" "KEY"  ]
        , url = "https://wordsapiv1.p.mashape.com/words/lime/rhymes"
        , body = Http.emptyBody
        , expect =
            expectJson
                (RemoteData.fromResult >> RhymesDataResponse)
                decodeWords
        , timeout = Nothing
        , tracker = Nothing

        }  


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RhymesDataResponse response ->
            ( { model | rhymesData = response }
            , Cmd.none
            )




---- VIEW ----

viewWords : Words -> Html msg
viewWords rhymesData =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [text rhymesData.word]
        , div [] [viewRhymes rhymesData.rhymes]
        ]
         

viewRhymes : Rhymes -> Html msg
viewRhymes posts =
    div [] ( List.map text posts.all) 

view : Model -> Html Msg
view model =
    case model.rhymesData of
        NotAsked ->
            div [] [ text "Not Asked" ]

        Loading ->
            div [] [ text "Loading" ]

        Failure _ ->
            div [] [ text "Failure" ]

        Success rhymesData ->
            viewWords rhymesData




---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init,fetchRhymesData)
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
