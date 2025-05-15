module Main exposing (Deck, DeckState(..), Model, Msg(..), Player, Route(..), main, view)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (attribute, class, disabled, lang, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Random
import Random.List
import ZipList as Zip


type Route
    = Blacks
    | Whites
    | Scores


type alias Deck =
    { blacks : List String
    , whites : List String
    }


type DeckState
    = Loading
    | ShufflingCards
    | Loaded
    | Failed String


type alias Player =
    { id : Int
    , score : Int
    , color : String
    }


type alias Model =
    { route : Route
    , state : DeckState
    , originalDeck : Maybe Deck
    , blacks : Maybe (Zip.ZipList String)
    , whites : Maybe (Zip.ZipList String)
    , players : List Player
    , nextPlayerId : Int
    }


listToZipList : List a -> Maybe (Zip.ZipList a)
listToZipList list =
    case list of
        head :: tail ->
            Just (Zip.new head tail)

        [] ->
            Nothing


type Msg
    = TabClicked Route
    | NextBlack
    | PrevBlack
    | NextWhite
    | PrevWhite
    | GotDeck (Result Http.Error Deck)
    | ShuffledCards (List String) (List String)
    | Reshuffle
    | AddPlayer
    | RemovePlayer Int
    | UpdatePlayerScore Int String
    | ResetScores


playerColors : List String
playerColors =
    [ "bg-red-500"
    , "bg-blue-500"
    , "bg-green-500"
    , "bg-yellow-500"
    , "bg-purple-500"
    , "bg-pink-500"
    , "bg-indigo-500"
    , "bg-orange-500"
    , "bg-teal-500"
    , "bg-cyan-500"
    ]


deckDecoder : Decoder Deck
deckDecoder =
    Decode.map2 Deck
        (Decode.field "black" (Decode.list Decode.string))
        (Decode.field "white" (Decode.list Decode.string))


getDeck : Cmd Msg
getDeck =
    Http.get
        { url = "/elm-against-humanity/deck.json"
        , expect = Http.expectJson GotDeck deckDecoder
        }


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { route = Blacks
      , state = Loading
      , originalDeck = Nothing
      , blacks = Nothing
      , whites = Nothing
      , players =
            [ { id = 1
              , score = 0
              , color = "bg-red-500"
              }
            , { id = 2
              , score = 0
              , color = "bg-blue-500"
              }
            ]
      , nextPlayerId = 3
      }
    , getDeck
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked route ->
            ( { model | route = route }, Cmd.none )

        NextBlack ->
            ( { model | blacks = Maybe.map Zip.forward model.blacks }, Cmd.none )

        PrevBlack ->
            ( { model | blacks = Maybe.map Zip.backward model.blacks }, Cmd.none )

        NextWhite ->
            ( { model | whites = Maybe.map Zip.forward model.whites }, Cmd.none )

        PrevWhite ->
            ( { model | whites = Maybe.map Zip.backward model.whites }, Cmd.none )

        GotDeck result ->
            case result of
                Ok loadedDeck ->
                    ( { model
                        | state = ShufflingCards
                        , originalDeck = Just loadedDeck
                      }
                    , Random.generate
                        (\( blacks, whites ) -> ShuffledCards blacks whites)
                        (Random.map2 Tuple.pair
                            (Random.List.shuffle loadedDeck.blacks |> Random.map (List.take 10))
                            (Random.List.shuffle loadedDeck.whites |> Random.map (List.take 10))
                        )
                    )

                Err _ ->
                    ( { model | state = Failed "Failed to load deck" }
                    , Cmd.none
                    )

        ShuffledCards blacks whites ->
            ( { model
                | state = Loaded
                , blacks = listToZipList blacks
                , whites = listToZipList whites
              }
            , Cmd.none
            )

        Reshuffle ->
            case model.originalDeck of
                Just originalDeck ->
                    ( { model | state = ShufflingCards }
                    , Random.generate
                        (\( blacks, whites ) -> ShuffledCards blacks whites)
                        (Random.map2 Tuple.pair
                            (Random.List.shuffle originalDeck.blacks |> Random.map (List.take 10))
                            (Random.List.shuffle originalDeck.whites |> Random.map (List.take 10))
                        )
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddPlayer ->
            let
                colorIndex : Int
                colorIndex =
                    modBy (List.length playerColors) (List.length model.players)

                color : String
                color =
                    Maybe.withDefault "bg-gray-500" (List.head (List.drop colorIndex playerColors))

                newPlayer : { id : Int, score : number, color : String }
                newPlayer =
                    { id = model.nextPlayerId
                    , score = 0
                    , color = color
                    }
            in
            ( { model
                | players = model.players ++ [ newPlayer ]
                , nextPlayerId = model.nextPlayerId + 1
              }
            , Cmd.none
            )

        RemovePlayer id ->
            if List.length model.players <= 2 then
                ( model, Cmd.none )

            else
                ( { model
                    | players = List.filter (\p -> p.id /= id) model.players
                  }
                , Cmd.none
                )

        UpdatePlayerScore id scoreStr ->
            ( { model
                | players =
                    List.map
                        (\p ->
                            if p.id == id then
                                { p | score = Maybe.withDefault p.score (String.toInt scoreStr) }

                            else
                                p
                        )
                        model.players
              }
            , Cmd.none
            )

        ResetScores ->
            ( { model
                | players = List.map (\p -> { p | score = 0 }) model.players
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center gap-2 p-4 h-dvh md:mx-auto md:w-9/12 lg:w-1/2" ]
        [ div [ class "w-full flex justify-between items-center" ]
            [ button
                [ class "btn btn-accent btn-sm"
                , onClick Reshuffle
                , disabled
                    (case model.state of
                        Loading ->
                            True

                        ShufflingCards ->
                            True

                        _ ->
                            False
                    )
                ]
                [ text "Shuffle" ]
            , tabs [ class "font-bold tabs-xs" ]
                [ tab [ active model.route Scores, onClick (TabClicked Scores) ] [ text "Scores" ]
                , tab [ active model.route Whites, onClick (TabClicked Whites) ] [ text "White Cards" ]
                , tab [ active model.route Blacks, onClick (TabClicked Blacks) ] [ text "Black Cards" ]
                ]
            ]
        , case model.state of
            Loading ->
                card [ class "bg-black rounded-box flex-1 w-full flex flex-col gap-4" ]
                    [ div [ class "skeleton h-4 w-full" ] []
                    , div [ class "skeleton h-4 w-full" ] []
                    , div [ class "skeleton h-4 w-full" ] []
                    , div [ class "skeleton h-4 w-full" ] []
                    , div [ class "skeleton h-4 w-32" ] []
                    ]

            ShufflingCards ->
                card [ class "bg-black rounded-box flex-1 w-full flex flex-col gap-4" ]
                    [ div [ class "skeleton h-4 w-full animate-pulse" ] []
                    , div [ class "skeleton h-4 w-full animate-pulse" ] []
                    , div [ class "skeleton h-4 w-full animate-pulse" ] []
                    , div [ class "skeleton h-4 w-full animate-pulse" ] []
                    , div [ class "skeleton h-4 w-32 animate-pulse" ] []
                    ]

            Failed error ->
                card [ class "bg-error rounded-box flex-1 w-full flex flex-col gap-4" ]
                    [ text error
                    ]

            Loaded ->
                screen model
        ]


screen : Model -> Html Msg
screen model =
    case model.route of
        Whites ->
            case model.whites of
                Just whites ->
                    screen_whites whites

                Nothing ->
                    div [] [ text "No white cards loaded" ]

        Blacks ->
            case model.blacks of
                Just blacks ->
                    screen_blacks blacks

                Nothing ->
                    div [] [ text "No black cards loaded" ]

        Scores ->
            div [ class "flex flex-col gap-2 flex-1 w-full" ]
                [ div [ class "bg-base-300 rounded-box flex-1 w-full p-6 flex flex-col" ]
                    [ div [ class "flex flex-col gap-4" ]
                        (List.map
                            (\player ->
                                div [ class "flex items-center gap-4" ]
                                    [ div [ class ("w-8 h-8 rounded-full shadow-inner " ++ player.color) ] []
                                    , div [ class "flex-1" ]
                                        [ input
                                            [ type_ "range"
                                            , class "range range-primary w-full"
                                            , Html.Attributes.min "0"
                                            , Html.Attributes.max "10"
                                            , value (String.fromInt player.score)
                                            , onInput (UpdatePlayerScore player.id)
                                            ]
                                            []
                                        ]
                                    , div [ class "text-2xl font-bold min-w-[2ch] text-center" ] [ text (String.fromInt player.score) ]
                                    , button
                                        [ class "btn btn-sm btn-error btn-circle"
                                        , onClick (RemovePlayer player.id)
                                        , disabled (List.length model.players <= 2)
                                        ]
                                        [ text "×" ]
                                    ]
                            )
                            model.players
                        )
                    ]
                , div [ class "w-full flex justify-between gap-4" ]
                    [ button
                        [ class "btn btn-warning btn-sm"
                        , onClick ResetScores
                        ]
                        [ text "Reset Scores" ]
                    , button
                        [ class "btn btn-sm btn-primary gap-2 self-center"
                        , onClick AddPlayer
                        , if List.length model.players < 10 then
                            disabled False

                          else
                            disabled True
                        ]
                        [ text "Add Player" ]
                    ]
                ]


screen_whites : Zip.ZipList String -> Html Msg
screen_whites (Zip.Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-white text-black" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-sm btn-secondary", onClick PrevWhite, enabled prev ] [ text "Prev" ]
            , button [ class "self-start btn btn-sm btn-secondary", onClick NextWhite, enabled next ] [ text "Next" ]
            ]
        ]


screen_blacks : Zip.ZipList String -> Html Msg
screen_blacks (Zip.Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-black text-white" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-sm btn-secondary", onClick PrevBlack, enabled prev ] [ text "Prev" ]
            , button [ class "self-start btn btn-sm btn-secondary", onClick NextBlack, enabled next ] [ text "Next" ]
            ]
        ]


enabled : List String -> Html.Attribute msg
enabled prev =
    case prev of
        [] ->
            disabled True

        _ ->
            disabled False


card : List (Html.Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    div
        (List.append [ class "p-6 font-bold text-4xl lg:text-5xl xl:text-6xl leading-12 lg:leading-16 xl:leading-24 w-full hyphen-auto", lang "en" ] attrs)
        content


deck : List (Html.Attribute msg) -> List (Html msg) -> Html msg
deck attrs cards =
    div (List.append [ class "carousel carousel-vertical rounded-box shadow" ] attrs) (List.map (\c -> div [ class "carousel-item h-full" ] [ c ]) cards)


tabs : List (Html.Attribute msg) -> List (Html msg) -> Html msg
tabs attrs items =
    div (List.append [ class "tabs tabs-box rounded-lg" ] attrs) items


tab : List (Html.Attribute msg) -> List (Html msg) -> Html msg
tab attrs label =
    button (List.append [ attribute "role" "tab", class "tab" ] attrs) label


active : Route -> Route -> Html.Attribute msg
active a b =
    class
        (if a == b then
            "tab-active"

         else
            ""
        )
