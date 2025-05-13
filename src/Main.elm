module Main exposing (Model, Msg(..), main, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class)
import Random


type alias Model =
    { prompt : String
    }


type Msg
    = Next String


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { prompt = "" }, random )


prompts : List String
prompts =
    [ "You're the host now. Make the rules. No one else knows."
    , "Act happy while I ruin your day."
    , "Charades, but the lie detector says you're wrong."
    , "Explain quantum physics using only cat noises."
    , "Tell a story backwards, starting with 'The End'."
    , "Be a weather reporter for emotions instead of weather."
    , "Describe your breakfast like it's breaking news."
    , "Pitch a movie where vegetables are the villains."
    , "Be a superhero whose power is making things mildly inconvenient."
    , "Translate this conversation into dolphin sounds."
    , "You're a GPS navigator having an existential crisis."
    , "Sell me air from different famous locations."
    , "Do an unboxing video of an imaginary time machine."
    , "You're a food critic reviewing water from different taps."
    , "Tell a dramatic story using only eyebrow movements."
    , "Give a TED talk about your sock drawer."
    , "Be a commentator for paint drying championships."
    , "Interview a chair about its life story."
    , "You're a detective solving the case of missing silence."
    , "Teach interpretive dance to imaginary penguins."
    , "Host a cooking show using only office supplies."
    , "Be a tour guide in your own pocket."
    , "Give a pep talk to a discouraged cloud."
    , "Re-enact the birth of the universe in 30 seconds."
    , "Review yesterday's nap like a movie critic."
    , "Be a sportscaster for plants growing."
    , "Pitch a reality show starring dust bunnies."
    , "Do stand-up comedy for a tough crowd of pillows."
    , "Conduct an orchestra of invisible musicians."
    , "Be a marriage counselor for left and right shoes."
    , "Write a love letter from your phone to its charger."
    , "Host a debate between morning people and night owls."
    , "Create a conspiracy theory about why socks disappear."
    , "Be a mime trapped in an invisible traffic jam."
    , "Write a LinkedIn profile for your coffee mug."
    , "Narrate a documentary about procrastination in real-time."
    , "Interview your future self about your past self's decisions."
    , "Be a detective investigating who stole the Wi-Fi signal."
    , "Organize a protest rally for neglected house plants."
    , "Create a dating profile for your favorite book character."
    , "Host a ted talk about the philosophy of rubber ducks."
    , "Be a sports commentator for a snail race marathon."
    , "Write a passive-aggressive note to gravity."
    , "Perform an interpretive dance of your browser history."
    , "Host a support group for abandoned shopping carts."
    , "Give a dramatic reading of your grocery list."
    , "Be a life coach for pessimistic rainbows."
    , "Create a podcast for an audience of house spiders."
    , "Write an acceptance speech for 'Most Average Person'."
    , "Design a theme park where nothing is fun."
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next prompt ->
            ( { model | prompt = prompt }, Cmd.none )


random : Cmd Msg
random =
    Random.generate Next (Random.uniform (Maybe.withDefault "" (List.head prompts)) (Maybe.withDefault [] (List.tail prompts)))


view : Model -> Html Msg
view _ =
    div [ class "flex flex-col items-center gap-8 p-6 h-dvh md:mx-auto md:w-3/4 lg:w-1/3" ]
        [ tabs [ class "font-bold self-end tabs-sm" ]
            [ tab [] [ text "White Deck" ]
            , tab [ class "tab-active" ] [ text "Black Deck" ]
            , tab [] [ text "Scores" ]
            ]
        , deck [ class "flex-1 w-full" ]
            [ card [ class "bg-black text-white" ] [ text "Do stand-up comedy for a tough crowd of pillows." ]
            , card [ class "bg-black text-white" ] [ text "Host a ted talk about the philosophy of rubber ducks." ]
            , card [ class "bg-black text-white" ] [ text "Write a LinkedIn profile for your coffee mug." ]
            ]
        , button [ class "self-start btn btn-secondary btn-lg rounded-3xl" ] [ text "Draw New Hand" ]
        ]


card : List (Html.Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    div
        (List.append [ class "p-6 text-5xl font-bold leading-16 w-full" ] attrs)
        content


deck : List (Html.Attribute msg) -> List (Html msg) -> Html msg
deck attrs cards =
    div (List.append [ class "carousel carousel-vertical rounded-box shadow" ] attrs) (List.map (\c -> div [ class "carousel-item h-full" ] [ c ]) cards)


tabs : List (Html.Attribute msg) -> List (Html msg) -> Html msg
tabs attrs items =
    div (List.append [ class "tabs tabs-box" ] attrs) items


tab : List (Html.Attribute msg) -> List (Html msg) -> Html msg
tab attrs label =
    div (List.append [ attribute "role" "tab", class "tab" ] attrs) label
