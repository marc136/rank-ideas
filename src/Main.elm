module Main exposing (Nodes(..), Step(..), main, pick, start)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { page : Page
    }


type Page
    = Collect CollectingIdea (List Idea)
    | Ranking (Step Idea)


type Idea
    = Simple String


type IdeaType
    = TitleOnly
    | TitleAndUrl
    | TitleAndDescription


type alias CollectingIdea =
    { kind : IdeaType
    , title : String
    , url : String
    , description : String
    }


newIdea : CollectingIdea
newIdea =
    CollectingIdea TitleOnly "" "" ""


type Step a
    = Compare (Nodes a) (Nodes a) (NodeList a) (List a)
    | Sorted (List a)


type Nodes a
    = Leaf a
    | Node a (NodeList a)


type alias NodeList a =
    List (Nodes a)


init : Model
init =
    { page = Collect newIdea []
    }


log : String -> a -> a
log str input =
    let
        _ =
            Debug.log ("\n" ++ str ++ " ") input
    in
    input


start : List a -> Step a
start list =
    case List.map Leaf list of
        [] ->
            Sorted []

        [ a ] ->
            Sorted (nodeToList a)

        a :: b :: rest ->
            Compare a b rest []


nodeToList : Nodes a -> List a
nodeToList node =
    case node of
        Leaf a ->
            [ a ]

        Node a child ->
            a :: nodeListToList child


nodeListToList : NodeList a -> List a
nodeListToList list =
    case list of
        [] ->
            []

        a :: rest ->
            nodeToList a ++ nodeListToList rest


pick : NodeList a -> List a -> Step a
pick list sorted =
    -- let _ = Debug.log "pick" ( list, sorted ) in
    case list of
        one :: two :: rest ->
            Compare one two rest sorted

        [ Node current children ] ->
            pick children (current :: sorted)

        [ Leaf a ] ->
            toSorted (a :: sorted)

        [] ->
            toSorted sorted


toSorted : List a -> Step a
toSorted reversedList =
    Sorted <| List.reverse reversedList



-- UPDATE


type Msg
    = NoOp
    | AddIdea
    | EditNewTitle String
    | StartRanking
    | PickFirst
    | PickSecond


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        EditNewTitle value ->
            case model.page of
                Collect idea list ->
                    { model | page = Collect { idea | title = value } list }

                _ ->
                    model

        AddIdea ->
            case model.page of
                Collect idea ideas ->
                    { model | page = Collect newIdea (collectIdea idea ideas) }

                _ ->
                    model

        StartRanking ->
            case model.page of
                Collect idea ideas ->
                    -- TODO if idea is not empty, query if it should be added
                    { model | page = Ranking <| start ideas }

                _ ->
                    model

        PickFirst ->
            case model.page of
                Ranking (Compare a b others sorted) ->
                    { model
                        | page =
                            pick (append a b :: others) sorted
                                |> Ranking
                    }

                _ ->
                    model

        PickSecond ->
            case model.page of
                Ranking (Compare a b others sorted) ->
                    { model
                        | page =
                            pick (append b a :: others) sorted
                                |> Ranking
                    }

                _ ->
                    model


collectIdea : CollectingIdea -> List Idea -> List Idea
collectIdea new existing =
    case
        trimCollectingIdea new
            |> convertIdea
    of
        Ok idea ->
            idea :: existing

        Err idea ->
            existing


trimCollectingIdea : CollectingIdea -> CollectingIdea
trimCollectingIdea { kind, title, url, description } =
    { kind = kind
    , title = String.trim title
    , url = String.trim url
    , description = String.trim description
    }


convertIdea : CollectingIdea -> Result CollectingIdea Idea
convertIdea idea =
    if idea.kind == TitleOnly then
        if String.isEmpty idea.title then
            Err idea

        else
            Ok <| Simple idea.title

    else
    -- TODO
    if
        String.isEmpty idea.title
    then
        Err idea

    else
        Ok <| Simple idea.title


append : Nodes a -> Nodes a -> Nodes a
append parent child =
    case parent of
        Leaf a ->
            Node a [ child ]

        Node a children ->
            Node a (child :: children)



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Collect latest ideas ->
            viewCollect latest ideas

        Ranking step ->
            viewRanking step


viewCollect : CollectingIdea -> List Idea -> Html Msg
viewCollect latest ideas =
    div [ class "collect" ]
        [ div [ class "new" ] <| collectNewIdea latest
        , ul [ class "ideas" ] <|
            List.map item ideas
        , button [ class "primary", onClick StartRanking ]
            [ text "Start ranking" ]
        ]


collectNewIdea : CollectingIdea -> List (Html Msg)
collectNewIdea idea =
    List.concat <|
        case idea.kind of
            TitleOnly ->
                [ editTitle idea
                , addButton
                ]

            TitleAndUrl ->
                [ editTitle idea

                -- , editUrl idea
                , addButton
                ]

            TitleAndDescription ->
                [ editTitle idea

                -- , editUrl idea
                -- , editDescription idea
                , addButton
                ]


editTitle : { a | title : String } -> List (Html Msg)
editTitle { title } =
    [ label [ for "new-title" ] [ text "Title" ]
    , input
        [ type_ "text"
        , onInput EditNewTitle
        , id "new-title"
        , value title
        ]
        []
    ]


addButton : List (Html Msg)
addButton =
    [ button [ class "primary", onClick AddIdea ] [ text "Add" ] ]


viewRanking : Step Idea -> Html Msg
viewRanking step =
    case step of
        Compare one two _ _ ->
            div [ class "pick" ]
                [ h2 [] [ text "Which one should have a higher priority?" ]
                , button [ onClick PickFirst ] [ text <| toString one ]
                , button [ onClick PickSecond ] [ text <| toString two ]
                ]

        Sorted list ->
            div [ class "result" ]
                [ h2 [] [ text "Your prioritization" ]
                , ol [] <|
                    List.map item list
                ]


toString : Nodes Idea -> String
toString node =
    case node of
        Leaf (Simple title) ->
            title

        Node (Simple title) _ ->
            title


item : Idea -> Html msg
item idea =
    case idea of
        Simple title ->
            li title


li : String -> Html msg
li caption =
    Html.li [] [ text caption ]
