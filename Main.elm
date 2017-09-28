module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Story =
    { id : String
    , score : Int
    , title : String
    , url : Maybe String
    }


type Query
    = Query (List Field)


type alias Field =
    { name : String
    , query : Query
    }


type alias Model =
    { stories : List Story
    , message : String
    }


type Msg
    = FetchHNTopStories (Result Http.Error (List Story))


field : String -> List Field -> Field
field name fileds =
    Field name (Query fileds)


topStoriesQuery : Query
topStoriesQuery =
    Query
        [ field "hn"
            [ field "topStories"
                [ field "id" []
                , field "score" []
                , field "title" []
                , field "url" [] -- we need the url
                , field "by"
                    [ field "id" [] ]
                ]
            ]
        ]


fieldToString : Field -> String
fieldToString { name, query } =
    name ++ " " ++ queryToString query


queryToString : Query -> String
queryToString (Query query) =
    if List.isEmpty query then
        ""
    else
        let
            str =
                List.map fieldToString query
                    |> List.foldr (++) ""
        in
            "{ " ++ str ++ " }"


storyDecoder : Decode.Decoder Story
storyDecoder =
    Pipeline.decode Story
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "score" Decode.int
        |> Pipeline.required "title" Decode.string
        |> Pipeline.optional "url" (Decode.nullable Decode.string) Nothing


request : Http.Request (List Story)
request =
    let
        encoded =
            queryToString topStoriesQuery
                |> Http.encodeUri

        decoder =
            Decode.at [ "data", "hn", "topStories" ] <|
                Decode.list storyDecoder
    in
        Http.get ("https://www.graphqlhub.com/graphql?query=" ++ encoded) decoder


init : ( Model, Cmd Msg )
init =
    { stories = []
    , message = "Fetching..."
    }
        ! [ Http.send FetchHNTopStories request ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchHNTopStories (Ok stories) ->
            { model | stories = stories, message = "Fetched" } ! []

        FetchHNTopStories (Err res) ->
            { model | message = toString res } ! []


listItem : Story -> Html Msg
listItem { id, score, title, url } =
    let
        url_ =
            Maybe.withDefault ("https://news.ycombinator.com/item?id=" ++ id) url
    in
        li [ class "flex bb b--black-10" ]
            [ span [ class "pa3 fw7 yellow f3" ] [ text (toString score) ]
            , span [ class "pa3" ]
                [ div [] [ a [ class "black link f4 fw4", href url_ ] [ text title ] ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        items =
            List.map listItem model.stories

        storiesList =
            ul [ class "list pl0" ] items
    in
        div [] [ text model.message, storiesList ]
