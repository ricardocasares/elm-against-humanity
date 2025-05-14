module Main exposing (Model, Msg(..), Route(..), main, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, disabled, lang)
import Html.Events exposing (onClick)
import ZipList exposing (ZipList(..), backward, forward)


type Route
    = Blacks
    | Whites
    | Scores


type alias Model =
    { route : Route
    , blacks : ZipList String
    , whites : ZipList String
    }


type Msg
    = TabClicked Route
    | NextBlack
    | PrevBlack
    | NextWhite
    | PrevWhite


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { route = Blacks, blacks = blacks, whites = whites }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked route ->
            ( { model | route = route }, Cmd.none )

        NextBlack ->
            ( { model | blacks = forward model.blacks }, Cmd.none )

        PrevBlack ->
            ( { model | blacks = backward model.blacks }, Cmd.none )

        NextWhite ->
            ( { model | whites = forward model.whites }, Cmd.none )

        PrevWhite ->
            ( { model | whites = backward model.whites }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center gap-8 p-6 h-dvh md:mx-auto md:w-9/12 lg:w-1/2" ]
        [ tabs [ class "font-bold self-end tabs-sm" ]
            [ tab [ active model.route Scores, onClick (TabClicked Scores) ] [ text "Scores" ]
            , tab [ active model.route Whites, onClick (TabClicked Whites) ] [ text "White Cards" ]
            , tab [ active model.route Blacks, onClick (TabClicked Blacks) ] [ text "Black Cards" ]
            ]
        , screen model
        ]


screen : Model -> Html Msg
screen model =
    case model.route of
        Whites ->
            screen_whites model.whites

        Blacks ->
            screen_blacks model.blacks

        Scores ->
            div [ class "bg-base-300 rounded-box flex-1 w-full text-4xl font-bold p-4 flex items-center justify-center" ] [ text "Working on it" ]


screen_whites : ZipList String -> Html Msg
screen_whites (Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-6 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-white text-black" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-secondary btn rounded-3xl", onClick PrevWhite, enabled prev ] [ text "Prev" ]
            , button [ class "self-start btn btn-secondary btn rounded-3xl", onClick NextWhite, enabled next ] [ text "Next" ]
            ]
        ]


screen_blacks : ZipList String -> Html Msg
screen_blacks (Zipper prev curr next) =
    div [ class "flex flex-col w-full gap-6 flex-1" ]
        [ deck [ class "flex-1 w-full" ] [ card [ class "bg-black text-white" ] [ text curr ] ]
        , div [ class "flex justify-between w-full" ]
            [ button [ class "self-start btn btn-secondary btn rounded-3xl", onClick PrevBlack, enabled prev ] [ text "Prev" ]
            , button [ class "self-start btn btn-secondary btn rounded-3xl", onClick NextBlack, enabled next ] [ text "Next" ]
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
    div (List.append [ class "tabs tabs-box" ] attrs) items


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


blacks : ZipList String
blacks =
    ZipList.new "Why can't I sleep at night?"
        [ "I got 99 problems but _______ ain't one."
        , "What's a girl's best friend?"
        , "What's that smell?"
        , "This is the way the world ends: not with a bang but with _______."
        , "What is Batman's guilty pleasure?"
        , "I'm sorry, Professor, but I couldn't complete my homework because of _______."
        , "What ended my last relationship?"
        , "What's that sound?"
        , "I drink to forget _______."
        , "What's the next Happy Meal® toy?"
        , "Here is the church, here is the steeple, open the doors, and there is _______."
        , "It's a pity that kids these days are all getting involved with _______."
        , "During sex, I like to think about _______."
        , "What's the most emo?"
        , "Instead of coal, Santa now gives bad children _______."
        , "What's the new fad diet?"
        , "When I am a billionaire, I shall erect a 50-foot statue to commemorate _______."
        , "What's the worst thing to say during a job interview?"
        , "What’s my secret power?"
        , "What gets better with age?"
        , "What's there a ton of in heaven?"
        , "Major League Baseball has banned _______ for giving players an unfair advantage."
        , "My mom freaked out when she found _______ in my browser history."
        , "What's the most problematic?"
        , "What never fails to liven up the party?"
        , "The class field trip was completely ruined by _______."
        , "When I was tripping on acid, _______ turned into _______."
        , "What would grandma find disturbing, yet oddly charming?"
        , "What did I bring back from Mexico?"
        , "What helps Obama unwind?"
        , "What did Vin Diesel eat for dinner?"
        , "Why am I sticky?"
        , "What will always get you laid?"
        , "What did I nickname my genitals?"
        , "What makes life worth living?"
        , "I never truly understood _______ until I encountered _______."
        , "How did I lose my virginity?"
        , "What's the next superhero/sidekick duo?"
        , "What’s fun until it gets weird?"
        ]


whites : ZipList String
whites =
    ZipList.new "A windmill full of corpses"
        [ "The entire Internet"
        , "An endless stream of diarrhea"
        , "Former President George W. Bush"
        , "A really cool hat"
        , "The screams... the terrible screams"
        , "A pyramid of severed heads"
        , "A mime having a stroke"
        , "The miracle of childbirth"
        , "A subscription to Men's Fitness"
        , "Bees?"
        , "Passive-aggressive Post-it notes"
        , "Waking up half-naked in a Denny's parking lot"
        , "My soul"
        , "An honest cop with nothing left to lose"
        , "The blood of Christ"
        , "Getting married, having a few kids, buying some stuff, retiring to Florida, and dying"
        , "A disappointing birthday party"
        , "An oversized lollipop"
        , "Flesh-eating bacteria"
        , "Doing the right thing"
        , "Poor life choices"
        , "A man on the brink of orgasm"
        , "The Big Bang"
        , "An army of skeletons"
        , "Chainsaws for hands"
        , "A lifetime of sadness"
        , "The invisible hand of the market"
        , "A stray pube"
        , "A surprising amount of hair"
        , "An erection that lasts longer than four hours"
        , "A tiny horse"
        , "A foul mouth"
        , "Gloryholes"
        , "A brain tumor"
        , "Tentacle porn"
        , "Friendly fire"
        , "Hope"
        , "Being rich"
        , "A gentle caress of the inner thigh"
        ]
