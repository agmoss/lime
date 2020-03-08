module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, img, input, li, text, ul)
import Html.Attributes exposing (id, placeholder, src, value)
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
    ( Model NotAsked "", Cmd.none )


type Msg
    = RhymesDataResponse (WebData Words)
    | UpdateWord String
    | GetRhymesData


fetchRhymesData : String -> Cmd Msg
fetchRhymesData input =
    Http.request
        { method = "GET"
        , headers = [ header "x-rapidapi-host" "wordsapiv1.p.rapidapi.com", header "x-rapidapi-key" "KEY" ]
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


renderList : List String -> Html msg
renderList lst =
    div [ id "list" ]
        [ ul []
            (List.map (\l -> li [] [ text l ]) lst)
        ]


viewWords : Words -> Html msg
viewWords rhymesData =
    div []
        [ h2 [] [ text (" Words that rhyme with: " ++ rhymesData.word) ]
        , div [] [ viewRhymes rhymesData.rhymes ]
        ]


viewRhymes : Rhymes -> Html msg
viewRhymes posts =
    renderList posts.all


asker : Model -> Html Msg
asker model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Lime " ]
        , input [ placeholder "Enter a word!", value model.inputWord, onInput UpdateWord ] []
        , button [ onClick GetRhymesData ] [ text "Search" ]
        ]


view : Model -> Html Msg
view model =
    case model.rhymesData of
        NotAsked ->
            div []
                [ asker model
                , text "Remote Data State: Not Asked"
                ]

        Loading ->
            div [] [ text "Remote Data State: Loading" ]

        Failure _ ->
            div []
                [ asker model
                , text "Remote Data State: Failure"
                ]

        Success rhymesData ->
            div []
                [ asker model
                , text "Remote Data State: Success"
                , viewWords rhymesData
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
