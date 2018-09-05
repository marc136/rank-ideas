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
    = Short String
    | ShortWithLink String String
    | Link String
    | Idea String String
    | IdeaWithLink String String String


type alias CollectingIdea =
    { title : String
    , link : String
    , description : String
    }


newIdea : CollectingIdea
newIdea =
    CollectingIdea "" "" ""


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
      -- Collecting ideas
    | AddIdea
    | EditNewDescription String
    | EditNewTitle String
    | EditNewUrl String
    | StartRanking
      -- Ranking ideas
    | PickFirst
    | PickSecond


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        EditNewDescription value ->
            case model.page of
                Collect idea list ->
                    { model | page = Collect { idea | description = value } list }

                _ ->
                    model

        EditNewTitle value ->
            case model.page of
                Collect idea list ->
                    { model | page = Collect { idea | title = value } list }

                _ ->
                    model

        EditNewUrl value ->
            case model.page of
                Collect idea list ->
                    { model | page = Collect { idea | link = value } list }

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
trimCollectingIdea { title, link, description } =
    { title = String.trim title
    , link = String.trim link
    , description = String.trim description
    }


convertIdea : CollectingIdea -> Result CollectingIdea Idea
convertIdea idea =
    case ( idea.title, idea.link, idea.description ) of
        ( "", "", "" ) ->
            Err idea

        ( title, "", "" ) ->
            Ok <| Short title

        ( "", link, "" ) ->
            Ok <| Link link

        ( "", "", description ) ->
            Ok <| Idea (getBeginning description) description

        ( title, link, "" ) ->
            Ok <| ShortWithLink title link

        ( title, "", description ) ->
            Ok <| Idea title description

        ( "", link, description ) ->
            Ok <| IdeaWithLink (getBeginning description) description link

        ( title, link, description ) ->
            Ok <| IdeaWithLink title description link


getBeginning : String -> String
getBeginning string =
    firstLine string
        |> ellipse maxTitleLength


firstLine : String -> String
firstLine string =
    String.lines string
        |> List.head
        |> Maybe.withDefault ""


ellipse : Int -> String -> String
ellipse max string =
    if String.length string > max then
        String.left (max - 3) string ++ "..."

    else
        string


maxTitleLength : Int
maxTitleLength =
    52


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
    [ editTitle idea.title
    , editUrl idea.link
    , editDescription idea.description
    , addButton
    ]
        |> List.concat


editTitle : String -> List (Html Msg)
editTitle value_ =
    [ label [ for "new-title" ] [ text "Title" ]
    , input
        [ type_ "text"
        , onInput EditNewTitle
        , id "new-title"
        , value value_
        ]
        []
    ]


editUrl : String -> List (Html Msg)
editUrl value_ =
    [ label [ for "new-url" ] [ text "URL" ]
    , input
        [ type_ "text"
        , onInput EditNewUrl
        , id "new-url"
        , value value_
        ]
        []
    ]


editDescription : String -> List (Html Msg)
editDescription value_ =
    [ label [ for "new-description" ] [ text "Description" ]
    , textarea
        [ onInput EditNewDescription
        , id "new-description"
        , value value_
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
                , toButton PickFirst (getIdea one)
                , toButton PickSecond (getIdea two)
                ]

        Sorted list ->
            div [ class "result" ]
                [ h2 [] [ text "Your prioritization" ]
                , ol [] <|
                    List.map item list
                ]


getIdea : Nodes Idea -> Idea
getIdea node =
    case node of
        Leaf idea ->
            idea

        Node idea _ ->
            idea


toButton : msg -> Idea -> Html msg
toButton msg idea =
    case idea of
        Short title ->
            div [ class "idea" ]
                [ p [ class "title" ] [ text title ]
                , button [ onClick msg ] [ text <| "Choose " ++ title ]
                ]

        ShortWithLink title url ->
            div [ class "idea" ]
                [ p [ class "title" ] [ text title ]
                , a [ href url, target "_blank" ] [ text url ]
                , button [ onClick msg ] [ text <| "Choose " ++ title ]
                ]

        Link url ->
            div [ class "idea" ]
                [ a [ href url, target "_blank" ] [ text url ]
                , button [ onClick msg ] [ text "Choose" ]
                ]

        Idea title description ->
            div [ class "idea" ]
                [ p [ class "title" ] [ text title ]
                , p [ class "description" ] [ text description ]
                , button [ onClick msg ] [ text <| "Choose " ++ title ]
                ]

        IdeaWithLink title description url ->
            div [ class "idea" ]
                [ p [ class "title" ] [ text title ]
                , a [ href url, target "_blank" ] [ text url ]
                , p [ class "description" ] [ text description ]
                , button [ onClick msg ] [ text <| "Choose " ++ title ]
                ]


item : Idea -> Html msg
item idea =
    case idea of
        Short title ->
            li title

        ShortWithLink title url ->
            li title

        Link url ->
            li url

        Idea title description ->
            li title

        IdeaWithLink title description url ->
            li title


li : String -> Html msg
li caption =
    Html.li [] [ text caption ]
