module Main exposing (Deck, DeckState(..), Model, Msg(..), Player, Route(..), TouchState, WakeLockStatus(..), main, view)

import Browser
import Html exposing (Html, button, div, h3, h4, input, label, text)
import Html.Attributes exposing (attribute, checked, class, disabled, lang, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import InteropDefinitions as IO
import InteropPorts as IO
import Json.Decode as Decode exposing (Decoder)
import Phosphor as I
import Random
import Random.List
import Translations exposing (Language(..), Translations, getTranslations, languageFromString, languageToString)
import ZipList as Zip


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
    , currentLanguage : Language
    , selectedBlackCard : Bool
    , selectedWhiteCard : Bool
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


listToZipList : List a -> Maybe (Zip.ZipList a)
listToZipList list =
    case list of
        head :: tail ->
            Just (Zip.new head tail)

        [] ->
            Nothing


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
    | LanguageDetected String
    | LanguageChanged Language
    | BlackCardClicked
    | WhiteCardClicked
    | TouchStart Float Float
    | TouchMove Float Float
    | TouchEnd
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


getDeck : String -> Language -> Cmd Msg
getDeck basePath language =
    let
        deckFile : String
        deckFile =
            case language of
                English ->
                    "deck-en.json"

                Spanish ->
                    "deck-es.json"

                Polish ->
                    "deck-pl.json"
    in
    Http.get
        { url = String.join "/" [ basePath, deckFile ]
        , expect = Http.expectJson GotDeck deckDecoder
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
      , currentLanguage = Spanish
      , selectedBlackCard = False
      , selectedWhiteCard = False
      , touchState = initialTouchState
      , basePath = flags.basePath
      }
    , Cmd.batch [ IO.fromElm IO.DetectLanguage, IO.fromElm IO.WakeLockCheck ]
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
                detectedLanguage : Language
                detectedLanguage =
                    languageFromString languageString

                newModel : Model
                newModel =
                    { model | currentLanguage = detectedLanguage }
            in
            ( { newModel | state = Loading }, getDeck model.basePath detectedLanguage )

        LanguageChanged language ->
            let
                newModel : Model
                newModel =
                    { model | currentLanguage = language }
            in
            ( { newModel | state = Loading }
            , Cmd.batch
                [ getDeck model.basePath language
                , IO.fromElm (IO.SaveLanguagePreference (languageToString language))
                ]
            )

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
                    ( { model | state = Failed (getTranslations model.currentLanguage).error }
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

        BlackCardClicked ->
            ( { model | selectedBlackCard = not model.selectedBlackCard }, Cmd.none )

        WhiteCardClicked ->
            ( { model | selectedWhiteCard = not model.selectedWhiteCard }, Cmd.none )

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
                            if not model.selectedBlackCard then
                                update NextBlack model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeRight, Blacks ) ->
                            if not model.selectedBlackCard then
                                update PrevBlack model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeLeft, Whites ) ->
                            if not model.selectedWhiteCard then
                                update NextWhite model

                            else
                                ( model, Cmd.none )

                        ( Just SwipeRight, Whites ) ->
                            if not model.selectedWhiteCard then
                                update PrevWhite model

                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { newModel | touchState = initialTouchState }, cmd )


{-| Helper function to render loading states for card screens (Blacks/Whites)
-}
renderCardScreen : DeckState -> Html Msg -> Html Msg
renderCardScreen state loadedContent =
    case state of
        Loading ->
            loadingSkeleton

        ShufflingCards ->
            loadingSkeleton

        Failed error ->
            errorCard error

        Loaded ->
            loadedContent


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
        [ text ("Error: " ++ error) ]


view : Model -> Html Msg
view model =
    let
        t : Translations.Translations
        t =
            getTranslations model.currentLanguage
    in
    div [ class "flex flex-col items-center gap-2 p-4 h-dvh md:mx-auto md:w-9/12 lg:w-1/2" ]
        [ div [ class "w-full flex items-center justify-end" ]
            [ tabs [ class "font-bold tabs-md" ]
                [ tab [ active model.route Settings, onClick (TabClicked Settings) ] [ I.wrench I.Regular |> I.toHtml [] ]
                , tab [ active model.route Help, onClick (TabClicked Help) ] [ I.question I.Regular |> I.toHtml [] ]
                , tab [ active model.route Scores, onClick (TabClicked Scores) ] [ I.trophy I.Regular |> I.toHtml [] ]
                , tab [ active model.route Whites, onClick (TabClicked Whites) ] [ text t.whiteCards ]
                , tab [ active model.route Blacks, onClick (TabClicked Blacks) ] [ text t.blackCards ]
                ]
            ]
        , case model.route of
            Blacks ->
                renderCardScreen model.state
                    (case model.blacks of
                        Just blacks ->
                            screen_blacks model.currentLanguage blacks model

                        Nothing ->
                            div [] [ text t.noBlackCardsLoaded ]
                    )

            Whites ->
                renderCardScreen model.state
                    (case model.whites of
                        Just whites ->
                            screen_whites model.currentLanguage whites model

                        Nothing ->
                            div [] [ text t.noWhiteCardsLoaded ]
                    )

            Scores ->
                screen model

            Settings ->
                screen model

            Help ->
                helpScreen model
        ]


helpScreen : Model -> Html Msg
helpScreen model =
    let
        t : Translations
        t =
            getTranslations model.currentLanguage
    in
    div [ class "flex flex-col gap-6 flex-1 w-full" ]
        [ div [ class "bg-base-300 rounded-box p-6 flex flex-col gap-4" ]
            [ h3 [ class "text-3xl font-bold" ] [ text t.howToPlay ]
            , div [ class "text-lg leading-relaxed" ]
                [ text t.gameOverview ]
            , div [ class "divider" ] []
            , h4 [ class "text-xl font-semibold" ] [ text t.playerRoles ]
            , div [ class "text-base leading-relaxed" ]
                [ text t.czarRole ]
            , div [ class "divider" ] []
            , h4 [ class "text-xl font-semibold" ] [ text t.gameplaySteps ]
            , div [ class "flex flex-col gap-3" ]
                [ div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "1" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.czarReadsCard ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "2" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.playersSelectCards ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "3" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.playersGivePhones ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "4" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.czarPicksBest ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "5" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.pointAwarded ]
                    ]
                , div [ class "flex items-start gap-3" ]
                    [ div [ class "badge badge-primary font-bold min-w-[2rem]" ] [ text "6" ]
                    , div [ class "text-base leading-relaxed" ] [ text t.newRoundStarts ]
                    ]
                ]
            , div [ class "divider" ] []
            , h4 [ class "text-xl font-semibold" ] [ text t.scoring ]
            , div [ class "text-base leading-relaxed" ]
                [ text "Players earn points by having their white cards chosen by the Card Czar. The game continues until players decide to stop, and the player with the most points wins!" ]
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


languageSelector : Language -> Html Msg
languageSelector currentLanguage =
    Html.select
        [ class "select select-bordered w-fit"
        , Html.Events.onInput
            (\selectedValue ->
                case selectedValue of
                    "en" ->
                        LanguageChanged English

                    "es" ->
                        LanguageChanged Spanish

                    "pl" ->
                        LanguageChanged Polish

                    _ ->
                        NoOp
            )
        ]
        [ Html.option
            [ Html.Attributes.value "en"
            , Html.Attributes.selected (currentLanguage == English)
            ]
            [ text "English" ]
        , Html.option
            [ Html.Attributes.value "es"
            , Html.Attributes.selected (currentLanguage == Spanish)
            ]
            [ text "Español" ]
        , Html.option
            [ Html.Attributes.value "pl"
            , Html.Attributes.selected (currentLanguage == Polish)
            ]
            [ text "Polski" ]
        ]


screen : Model -> Html Msg
screen model =
    let
        t : Translations
        t =
            getTranslations model.currentLanguage
    in
    case model.route of
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
                        [ text t.reset ]
                    , button
                        [ class "btn btn-sm btn-primary gap-2 self-center"
                        , onClick AddPlayer
                        , if List.length model.players < 10 then
                            disabled False

                          else
                            disabled True
                        ]
                        [ text t.add ]
                    ]
                ]

        Settings ->
            div [ class "flex flex-col gap-2 flex-1 w-full" ]
                [ div [ class "bg-base-300 rounded-box flex-1 w-full p-6 flex flex-col" ]
                    [ h3 [ class "text-3xl font-bold mb-10" ] [ text t.settings ]
                    , div [ class "flex flex-col gap-4" ]
                        [ label [ class "flex items-center justify-between w-full" ]
                            [ text t.selectLanguage
                            , languageSelector model.currentLanguage
                            ]
                        , label [ class "flex items-center justify-between w-full" ]
                            [ text t.preventScreenDimming
                            , wakeLockButton model
                            ]
                        ]
                    ]
                ]

        -- These cases should never be reached as they're handled in the main view
        Whites ->
            div [] []

        Blacks ->
            div [] []

        Help ->
            div [] []


screen_whites : Language -> Zip.ZipList String -> Model -> Html Msg
screen_whites _ (Zip.Zipper _ curr _) model =
    let
        cardClasses : String
        cardClasses =
            if model.selectedWhiteCard then
                "bg-white text-black border-4 border-secondary shadow-lg"

            else
                "bg-white text-black"

        touchEvents : List (Html.Attribute Msg)
        touchEvents =
            if model.selectedWhiteCard then
                [ onClick WhiteCardClicked ]

            else
                [ onClick WhiteCardClicked
                , onTouchStart TouchStart
                , onTouchMove TouchMove
                , onTouchEnd TouchEnd
                ]
    in
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ]
            [ card (class cardClasses :: touchEvents) [ text curr ] ]
        ]


screen_blacks : Language -> Zip.ZipList String -> Model -> Html Msg
screen_blacks _ (Zip.Zipper _ curr _) model =
    let
        cardClasses : String
        cardClasses =
            if model.selectedBlackCard then
                "bg-black text-white border-4 border-secondary shadow-lg"

            else
                "bg-black text-white"

        touchEvents : List (Html.Attribute Msg)
        touchEvents =
            if model.selectedBlackCard then
                [ onClick BlackCardClicked ]

            else
                [ onClick BlackCardClicked
                , onTouchStart TouchStart
                , onTouchMove TouchMove
                , onTouchEnd TouchEnd
                ]
    in
    div [ class "flex flex-col w-full gap-2 flex-1" ]
        [ deck [ class "flex-1 w-full" ]
            [ card (class cardClasses :: touchEvents) [ text curr ] ]
        ]


card : List (Html.Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    div
        (List.append [ class "p-6 border-4 border-content font-bold rounded-2xl text-4xl lg:text-5xl xl:text-6xl leading-12 lg:leading-16 xl:leading-24 w-full hyphen-auto", lang "en" ] attrs)
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
