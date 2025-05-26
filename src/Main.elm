module Main exposing (Deck, Model, Msg(..), Player, RemoteDeck(..), Route(..), TouchState, WakeLockStatus(..), main, view)

import Browser
import Html exposing (Html, button, div, h3, h4, input, label, option, text)
import Html.Attributes as A exposing (attribute, checked, class, disabled, lang, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import InteropDefinitions as IO
import InteropPorts as IO
import Json.Decode as Decode exposing (Decoder)
import Phosphor as I
import Random
import Random.List
import Translations as T
import ZipList as Zip exposing (ZipList(..))


type Route
    = Blacks
    | Whites
    | Scores
    | Settings
    | Help


type alias Deck =
    { blacks : List String
    , whites : List String
    }


type RemoteDeck
    = DeckLoaded
    | DeckLoading
    | DeckLoadingError String


type alias Player =
    { id : Int
    , score : Int
    , color : String
    }


type alias Model =
    { route : Route
    , deck : RemoteDeck
    , blacks : Maybe (Zip.ZipList String)
    , whites : Maybe (Zip.ZipList String)
    , players : List Player
    , nextPlayerId : Int
    , wakeLockStatus : WakeLockStatus
    , currentLanguage : T.Language
    , i18n : T.Translations
    , cardSelected : Bool
    , touchState : TouchState
    , basePath : String
    }


type alias TouchState =
    { startX : Float
    , startY : Float
    , currentX : Float
    , currentY : Float
    , isActive : Bool
    }


type WakeLockStatus
    = WakeLockUnknown
    | WakeLockSupported
    | WakeLockNotSupported
    | WakeLockActive
    | WakeLockInactive
    | WakeLockFailed String


type SwipeDirection
    = SwipeLeft
    | SwipeRight


detectSwipe : TouchState -> Maybe SwipeDirection
detectSwipe touch =
    let
        deltaX : Float
        deltaX =
            touch.currentX - touch.startX

        deltaY : Float
        deltaY =
            touch.currentY - touch.startY

        minSwipeDistance : Float
        minSwipeDistance =
            50.0

        maxVerticalTolerance : Float
        maxVerticalTolerance =
            100.0
    in
    if abs deltaX > minSwipeDistance && abs deltaY < maxVerticalTolerance then
        if deltaX > 0 then
            Just SwipeRight

        else
            Just SwipeLeft

    else
        Nothing


initialTouchState : TouchState
initialTouchState =
    { startX = 0
    , startY = 0
    , currentX = 0
    , currentY = 0
    , isActive = False
    }


onTouchStart : (Float -> Float -> msg) -> Html.Attribute msg
onTouchStart msg =
    Html.Events.on "touchstart"
        (Decode.map2 msg
            (Decode.at [ "touches", "0", "clientX" ] Decode.float)
            (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        )


onTouchMove : (Float -> Float -> msg) -> Html.Attribute msg
onTouchMove msg =
    Html.Events.on "touchmove"
        (Decode.map2 msg
            (Decode.at [ "touches", "0", "clientX" ] Decode.float)
            (Decode.at [ "touches", "0", "clientY" ] Decode.float)
        )


onTouchEnd : msg -> Html.Attribute msg
onTouchEnd msg =
    Html.Events.on "touchend" (Decode.succeed msg)


type Msg
    = TabClicked Route
    | NextBlack
    | PrevBlack
    | NextWhite
    | PrevWhite
    | RemoteDeckLoaded (Result Http.Error Deck)
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
    | LanguageDetected String
    | LanguageChanged T.Language
    | TouchStart Float Float
    | TouchMove Float Float
    | TouchEnd
    | CardSelected
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


getDeck : String -> T.Language -> Cmd Msg
getDeck basePath language =
    let
        deckFile : String
        deckFile =
            case language of
                T.English ->
                    "deck-en.json"

                T.Spanish ->
                    "deck-es.json"

                T.Polish ->
                    "deck-pl.json"
    in
    Http.get
        { url = basePath ++ deckFile
        , expect = Http.expectJson RemoteDeckLoaded deckDecoder
        }


main : Program IO.Flags Model Msg
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

                            IO.LanguageDetected language ->
                                LanguageDetected language

                    Err _ ->
                        NoOp
            )


init : IO.Flags -> ( Model, Cmd Msg )
init flags =
    ( { route = Blacks
      , deck = DeckLoading
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
      , currentLanguage = T.English
      , cardSelected = False
      , touchState = initialTouchState
      , basePath = flags.basePath
      , i18n = T.englishTranslations
      }
    , Cmd.batch [ IO.fromElm IO.DetectLanguage, IO.fromElm IO.WakeLockCheck ]
      -- , Cmd.none
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

        LanguageDetected languageString ->
            let
                detected : T.Language
                detected =
                    T.languageFromString languageString
            in
            ( { model | currentLanguage = detected, i18n = T.translate detected }, getDeck model.basePath detected )

        LanguageChanged language ->
            ( { model | currentLanguage = language, i18n = T.translate language }
            , Cmd.batch
                [ getDeck model.basePath language
                , IO.fromElm (IO.SaveLanguagePreference (T.languageToString language))
                ]
            )

        TabClicked route ->
            ( { model | route = route, cardSelected = False }, Cmd.none )

        NextBlack ->
            ( { model | blacks = Maybe.map Zip.forward model.blacks }, Cmd.none )

        PrevBlack ->
            ( { model | blacks = Maybe.map Zip.backward model.blacks }, Cmd.none )

        NextWhite ->
            ( { model | whites = Maybe.map Zip.forward model.whites }, Cmd.none )

        PrevWhite ->
            ( { model | whites = Maybe.map Zip.backward model.whites }, Cmd.none )

        RemoteDeckLoaded result ->
            case result of
                Ok data ->
                    ( { model | deck = DeckLoaded }, randomize data )

                Err _ ->
                    ( { model | deck = DeckLoadingError (T.translate model.currentLanguage).error }
                    , Cmd.none
                    )

        ShuffledCards blacks whites ->
            ( { model
                | blacks = Zip.fromList blacks
                , whites = Zip.fromList whites
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

                newPlayer : Player
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

        TouchStart x y ->
            ( { model
                | touchState =
                    { startX = x
                    , startY = y
                    , currentX = x
                    , currentY = y
                    , isActive = True
                    }
              }
            , Cmd.none
            )

        TouchMove x y ->
            if model.touchState.isActive then
                ( { model
                    | touchState =
                        { startX = model.touchState.startX
                        , startY = model.touchState.startY
                        , currentX = x
                        , currentY = y
                        , isActive = True
                        }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        TouchEnd ->
            let
                swipeDirection : Maybe SwipeDirection
                swipeDirection =
                    detectSwipe model.touchState

                ( newModel, cmd ) =
                    case ( swipeDirection, model.route ) of
                        ( Just SwipeLeft, Blacks ) ->
                            if not model.cardSelected then
                                update NextBlack model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeRight, Blacks ) ->
                            if not model.cardSelected then
                                update PrevBlack model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeLeft, Whites ) ->
                            if not model.cardSelected then
                                update NextWhite model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeRight, Whites ) ->
                            if not model.cardSelected then
                                update PrevWhite model

                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { newModel | touchState = initialTouchState }, cmd )

        CardSelected ->
            ( { model | cardSelected = not model.cardSelected }, Cmd.none )


randomize : Deck -> Cmd Msg
randomize data =
    Random.generate
        (\( blacks, whites ) -> ShuffledCards blacks whites)
        (Random.map2 Tuple.pair
            (Random.List.shuffle data.blacks |> Random.map (List.take 40))
            (Random.List.shuffle data.whites |> Random.map (List.take 40))
        )


{-| Skeleton loading screen
-}
loadingSkeleton : Html Msg
loadingSkeleton =
    card [ class "bg-black rounded-box flex-1 w-full flex flex-col gap-4" ]
        [ div [ class "skeleton h-4 w-full" ] []
        , div [ class "skeleton h-4 w-full" ] []
        , div [ class "skeleton h-4 w-full" ] []
        , div [ class "skeleton h-4 w-full" ] []
        , div [ class "skeleton h-4 w-32" ] []
        ]


{-| Error card display
-}
errorCard : String -> Html Msg
errorCard error =
    card [ class "bg-error rounded-box flex-1 w-full flex flex-col gap-4" ]
        [ text error ]


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center gap-2 p-4 h-dvh md:mx-auto md:w-9/12 lg:w-1/2" ]
        [ div [ class "w-full flex items-center justify-end" ]
            [ tabs [ class "font-bold tabs-sm" ]
                [ tab [ active model.route Settings, onClick (TabClicked Settings) ] [ I.wrench I.Regular |> I.toHtml [] ]
                , tab [ active model.route Help, onClick (TabClicked Help) ] [ I.question I.Regular |> I.toHtml [] ]
                , tab [ active model.route Scores, onClick (TabClicked Scores) ] [ I.trophy I.Regular |> I.toHtml [] ]
                , tab [ active model.route Whites, onClick (TabClicked Whites) ] [ text model.i18n.whiteCards ]
                , tab [ active model.route Blacks, onClick (TabClicked Blacks) ] [ text model.i18n.blackCards ]
                ]
            ]
        , case model.route of
            Blacks ->
                case model.deck of
                    DeckLoaded ->
                        case model.blacks of
                            Nothing ->
                                errorCard "Black cards was Nothing"

                            Just (Zipper _ curr _) ->
                                let
                                    selected : Html.Attribute msg
                                    selected =
                                        if model.cardSelected then
                                            class "border-4 border-secondary"

                                        else
                                            class ""
                                in
                                card [ class "bg-black flex-1 flex flex-col", selected ]
                                    [ div [ class "flex flex-1" ] [ text curr ]
                                    , div [ class "self-center animate animate-pulse" ] [ I.handTap I.Regular |> I.toHtml [] ]
                                    ]

                    DeckLoading ->
                        loadingSkeleton

                    DeckLoadingError error ->
                        errorCard error

            Whites ->
                case model.deck of
                    DeckLoaded ->
                        case model.whites of
                            Nothing ->
                                errorCard "White cards was Nothing"

                            Just (Zipper _ curr _) ->
                                let
                                    selected : Html.Attribute msg
                                    selected =
                                        if model.cardSelected then
                                            class "border-4 border-secondary"

                                        else
                                            class ""
                                in
                                card [ class "bg-white text-black flex-1 flex flex-col", selected ]
                                    [ div [ class "flex flex-1" ] [ text curr ]
                                    , div [ class "self-center animate animate-pulse" ] [ I.handTap I.Regular |> I.toHtml [] ]
                                    ]

                    DeckLoading ->
                        loadingSkeleton

                    DeckLoadingError error ->
                        errorCard error

            Scores ->
                scoresScreen model

            Settings ->
                div [ class "flex flex-col gap-2 flex-1 w-full" ]
                    [ div [ class "bg-base-300 rounded-box flex-1 w-full p-6 flex flex-col" ]
                        [ h3 [ class "text-3xl font-bold mb-10" ] [ text model.i18n.settings ]
                        , div [ class "flex flex-col gap-4" ]
                            [ label [ class "flex items-center justify-between w-full" ]
                                [ text model.i18n.selectLanguage
                                , languageSelector model.currentLanguage
                                ]
                            , label [ class "flex items-center justify-between w-full" ]
                                [ text model.i18n.preventScreenDimming
                                , wakeLockButton model
                                ]
                            ]
                        ]
                    ]

            Help ->
                helpScreen model
        ]


scoresScreen : Model -> Html Msg
scoresScreen model =
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
                                    , A.min "0"
                                    , A.max "10"
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
                [ text model.i18n.reset ]
            , button
                [ class "btn btn-sm btn-primary gap-2 self-center"
                , onClick AddPlayer
                , if List.length model.players < 10 then
                    disabled False

                  else
                    disabled True
                ]
                [ text model.i18n.add ]
            ]
        ]


helpScreen : Model -> Html Msg
helpScreen model =
    div [ class "flex flex-col gap-6 flex-1 w-full" ]
        [ div [ class "bg-base-300 rounded-box p-6 flex flex-col gap-4" ]
            [ h3 [ class "text-3xl font-bold" ] [ text model.i18n.howToPlay ]
            , div [ class "text-lg leading-relaxed" ]
                [ text model.i18n.gameOverview ]
            , div [ class "divider" ] []
            , h4 [ class "text-xl font-semibold" ] [ text model.i18n.playerRoles ]
            , div [ class "text-base leading-relaxed" ]
                [ text model.i18n.czarRole ]
            , div [ class "divider" ] []
            , h4 [ class "text-xl font-semibold" ] [ text model.i18n.gameplaySteps ]
            , div [ class "flex flex-col gap-3" ]
                [ div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "1" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.czarReadsCard ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "2" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.playersSelectCards ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "3" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.playersGivePhones ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "4" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.czarPicksBest ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "5" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.pointAwarded ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "6" ]
                    , div [ class "text-base leading-relaxed" ] [ text model.i18n.newRoundStarts ]
                    ]
                ]
            ]
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


languageSelector : T.Language -> Html Msg
languageSelector currentLanguage =
    Html.select
        [ class "select select-bordered w-fit"
        , Html.Events.onInput
            (\selectedValue ->
                case selectedValue of
                    "en" ->
                        LanguageChanged T.English

                    "es" ->
                        LanguageChanged T.Spanish

                    "pl" ->
                        LanguageChanged T.Polish

                    _ ->
                        NoOp
            )
        ]
        [ option
            [ A.value "en"
            , A.selected (currentLanguage == T.English)
            ]
            [ text "English" ]
        , Html.option
            [ A.value "es"
            , A.selected (currentLanguage == T.Spanish)
            ]
            [ text "Español" ]
        , Html.option
            [ A.value "pl"
            , A.selected (currentLanguage == T.Polish)
            ]
            [ text "Polski" ]
        ]


card : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
card attrs content =
    div
        (List.append
            [ class "p-6 border-4 border-content font-bold rounded-2xl text-4xl lg:text-5xl xl:text-6xl leading-12 lg:leading-16 xl:leading-24 w-full hyphen-auto"
            , lang "en"
            , onClick CardSelected
            , onTouchStart TouchStart
            , onTouchMove TouchMove
            , onTouchEnd TouchEnd
            ]
            attrs
        )
        content


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
