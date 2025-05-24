module Main exposing (Deck, DeckState(..), Model, Msg(..), Player, Route(..), WakeLockStatus(..), main, view)

import Browser
import Html exposing (Html, button, div, h3, input, label, text)
import Html.Attributes exposing (attribute, checked, class, disabled, lang, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import InteropDefinitions as IO
import InteropPorts as IO
import Json.Decode as Decode exposing (Decoder)
import Phosphor as I
import Random
import Random.List
import ZipList as Zip


type Route
    = Blacks
    | Whites
    | Scores
    | Settings


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
    , deck : Maybe Deck
    , blacks : Maybe (Zip.ZipList String)
    , whites : Maybe (Zip.ZipList String)
    , players : List Player
    , nextPlayerId : Int
    , wakeLockStatus : WakeLockStatus
    }


type WakeLockStatus
    = WakeLockUnknown
    | WakeLockSupported
    | WakeLockNotSupported
    | WakeLockActive
    | WakeLockInactive
    | WakeLockFailed String


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
    | AddPlayer
    | RemovePlayer Int
    | UpdatePlayerScore Int String
    | ResetScores
    | WakeLockToggle
    | WakeLockAcquired
    | WakeLockAvailable
    | WakeLockReleased
    | WakeLockError String
    | NoOp


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
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    IO.toElm
        |> Sub.map
            (\result ->
                case result of
                    Ok data ->
                        case data of
                            IO.WakeLockAvailable ->
                                WakeLockAvailable

                            IO.WakeLockAcquired ->
                                WakeLockAcquired

                            IO.WakeLockReleased ->
                                WakeLockReleased

                            IO.WakeLockError error ->
                                WakeLockError error

                    Err _ ->
                        NoOp
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { route = Blacks
      , deck = Nothing
      , state = Loading
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
      , wakeLockStatus = WakeLockUnknown
      }
    , Cmd.batch [ getDeck, IO.fromElm IO.WakeLockCheck ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WakeLockAvailable ->
            ( { model | wakeLockStatus = WakeLockSupported }, Cmd.none )

        WakeLockToggle ->
            case model.wakeLockStatus of
                WakeLockActive ->
                    ( model, IO.fromElm IO.WakeLockRelease )

                _ ->
                    ( model, IO.fromElm IO.WakeLockAcquire )

        WakeLockAcquired ->
            ( { model | wakeLockStatus = WakeLockActive }, Cmd.none )

        WakeLockReleased ->
            ( { model | wakeLockStatus = WakeLockInactive }, Cmd.none )

        WakeLockError error ->
            ( { model | wakeLockStatus = WakeLockFailed error }, Cmd.none )

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
                        , deck = Just loadedDeck
                      }
                    , Random.generate
                        (\( blacks, whites ) -> ShuffledCards blacks whites)
                        (Random.map2 Tuple.pair
                            (Random.List.shuffle loadedDeck.blacks |> Random.map (List.take 40))
                            (Random.List.shuffle loadedDeck.whites |> Random.map (List.take 40))
                        )
                    )

                Err _ ->
                    ( { model | state = Failed "No se pudo cargar el mazo" }
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
        [ div [ class "w-full flex items-center justify-end" ]
            [ tabs [ class "font-bold tabs-sm" ]
                [ tab [ active model.route Settings, onClick (TabClicked Settings) ] [ I.wrench I.Regular |> I.toHtml [] ]
                , tab [ active model.route Scores, onClick (TabClicked Scores) ] [ text "Puntos" ]
                , tab [ active model.route Whites, onClick (TabClicked Whites) ] [ text "Cartas Blancas" ]
                , tab [ active model.route Blacks, onClick (TabClicked Blacks) ] [ text "Cartas Negras" ]
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
                    [ text ("Error: " ++ error) ]

            Loaded ->
                screen model
        ]


wakeLockButton : Model -> Html Msg
wakeLockButton model =
    let
        attrs : List (Html.Attribute Msg)
        attrs =
            case model.wakeLockStatus of
                WakeLockUnknown ->
                    [ class "toggle toggle-info", disabled True, checked False ]

                WakeLockSupported ->
                    [ class "toggle", disabled False, checked False, onCheck (\_ -> WakeLockToggle) ]

                WakeLockNotSupported ->
                    [ class "toggle toggle-error", disabled True, checked False, title "Not supported on your device" ]

                WakeLockActive ->
                    [ class "toggle toggle-success", disabled False, checked True, onCheck (\_ -> WakeLockToggle) ]

                WakeLockInactive ->
                    [ class "toggle toggle-primary", disabled False, checked False, onCheck (\_ -> WakeLockToggle) ]

                WakeLockFailed err ->
                    [ class "toggle toggle-error", disabled True, checked False, title err ]
    in
    input (attrs ++ [ type_ "checkbox" ]) [ I.screencast I.Regular |> I.toHtml [] ]


screen : Model -> Html Msg
screen model =
    case model.route of
        Whites ->
            case model.whites of
                Just whites ->
                    screen_whites whites

                Nothing ->
                    div [] [ text "No se cargaron cartas blancas" ]

        Blacks ->
            case model.blacks of
                Just blacks ->
                    screen_blacks blacks

                Nothing ->
                    div [] [ text "No se cargaron cartas negras" ]

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
                        [ text "Reiniciar" ]
                    , button
                        [ class "btn btn-sm btn-primary gap-2 self-center"
                        , onClick AddPlayer
                        , if List.length model.players < 10 then
                            disabled False

                          else
                            disabled True
                        ]
                        [ text "Agregar" ]
                    ]
                ]

        Settings ->
            div [ class "flex flex-col gap-2 flex-1 w-full" ]
                [ div [ class "bg-base-300 rounded-box flex-1 w-full p-6 flex flex-col" ]
                    [ h3 [ class "text-3xl font-bold mb-10" ] [ text "Settings" ]
                    , div [ class "flex flex-col gap-2" ]
                        [ label [ class "flex items-center justify-between w-full text-xl" ]
                            [ text "Prevent screen dimming"
                            , wakeLockButton model
                            ]
                        ]
                    ]
                ]


screen_whites : Zip.ZipList String -> Html Msg
screen_whites (Zip.Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-white text-black" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-sm btn-secondary", onClick PrevWhite, enabled prev ] [ text "Anterior" ]
            , button [ class "self-start btn btn-sm btn-secondary", onClick NextWhite, enabled next ] [ text "Siguiente" ]
            ]
        ]


screen_blacks : Zip.ZipList String -> Html Msg
screen_blacks (Zip.Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-black text-white" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-sm btn-secondary", onClick PrevBlack, enabled prev ] [ text "Anterior" ]
            , button [ class "self-start btn btn-sm btn-secondary", onClick NextBlack, enabled next ] [ text "Siguiente" ]
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
