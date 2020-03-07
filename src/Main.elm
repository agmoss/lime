module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectJson, header, request)
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
        (D.field "word" D.string)
        (D.field "rhymes" decodeRhymes)


decodeRhymes : Decoder Rhymes
decodeRhymes =
    D.map Rhymes
        (D.field "all" stringArrayDecoder)



---- MODEL ----


type alias Model =
    { rhymesData : WebData Words
    , inputWord : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NotAsked "word", Cmd.none )


type Msg
    = RhymesDataResponse (WebData Words)
    | UpdateWord String
    | GetRhymesData


fetchRhymesData : String -> Cmd Msg
fetchRhymesData input =
    Http.request
        { method = "GET"
        , headers = [ header "x-rapidapi-host" "wordsapiv1.p.rapidapi.com", header "x-rapidapi-key" "a7f48b639amsh91af3afe2b70fcfp1d772bjsnc38a2bbeb654" ]
        , url = "https://wordsapiv1.p.mashape.com/words/" ++ input ++ "/rhymes"
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

        UpdateWord word ->
            ( { model | inputWord = word }, Cmd.none )

        GetRhymesData ->
            ( model, fetchRhymesData model.inputWord )



---- VIEW ----


viewWords : Words -> Html msg
viewWords rhymesData =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text rhymesData.word ]
        , div [] [ viewRhymes rhymesData.rhymes ]
        ]


viewRhymes : Rhymes -> Html msg
viewRhymes posts =
    div [] (List.map text posts.all)


view : Model -> Html Msg
view model =
    case model.rhymesData of
        NotAsked ->
            asker model

        Loading ->
            div [] [ text "Loading" ]

        Failure _ ->
            div [] [ text "Failure" ]

        Success rhymesData ->
            viewWords rhymesData


asker : Model -> Html Msg
asker model =
    div []
        [ img [ src "/logo.svg" ] []
        , button [ onClick GetRhymesData ] [ text "Search" ]
        , div []
            [ input [ placeholder "Word", value model.inputWord, onInput UpdateWord ] []
            , div [] [ text "Loading" ]
            , div [] [ text model.inputWord ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
