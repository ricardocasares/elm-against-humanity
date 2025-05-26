module Translations exposing (Language(..), Translations, languageFromString, languageToString, translate)


type Language
    = English
    | Spanish
    | Polish


type alias Translations =
    { -- Tab labels
      settings : String
    , whiteCards : String
    , blackCards : String
    , help : String

    -- Settings page
    , selectLanguage : String
    , preventScreenDimming : String

    -- Scores page
    , reset : String
    , add : String

    -- Card navigation
    , previous : String
    , next : String

    -- Help page
    , howToPlay : String
    , gameOverview : String
    , playerRoles : String
    , gameplaySteps : String
    , czarRole : String
    , czarReadsCard : String
    , playersSelectCards : String
    , playersGivePhones : String
    , czarPicksBest : String
    , pointAwarded : String
    , newRoundStarts : String

    -- Error messages
    , noWhiteCardsLoaded : String
    , noBlackCardsLoaded : String
    , error : String
    }


translate : Language -> Translations
translate language =
    case language of
        English ->
            englishTranslations

        Spanish ->
            spanishTranslations

        Polish ->
            polishTranslations


englishTranslations : Translations
englishTranslations =
    { settings = "Settings"
    , whiteCards = "White Cards"
    , blackCards = "Black Cards"
    , help = "Help"
    , selectLanguage = "Language"
    , preventScreenDimming = "Prevent screen dimming"
    , reset = "Reset"
    , add = "Add"
    , previous = "Previous"
    , next = "Next"
    , howToPlay = "How to Play"
    , gameOverview = "Elm Against Humanity is a party game where players combine cards to create funny or outrageous combinations."
    , playerRoles = "Player Roles"
    , gameplaySteps = "How to Play"
    , czarRole = "One player is the Card Czar (this role rotates each round)."
    , czarReadsCard = "The Card Czar reads a black card aloud to all players."
    , playersSelectCards = "All other players select one white card from their hand by tapping on it."
    , playersGivePhones = "Players give their phones to the Card Czar so they can see all the selected white cards."
    , czarPicksBest = "The Card Czar picks the best white card combination in their opinion."
    , pointAwarded = "The player who submitted the chosen white card gets one point."
    , newRoundStarts = "A new round begins with the next player becoming the Card Czar."
    , noWhiteCardsLoaded = "No white cards loaded"
    , noBlackCardsLoaded = "No black cards loaded"
    , error = "An error occurred while loading the deck."
    }


spanishTranslations : Translations
spanishTranslations =
    { settings = "Configuración"
    , whiteCards = "Cartas Blancas"
    , blackCards = "Cartas Negras"
    , help = "Ayuda"
    , selectLanguage = "Idioma"
    , preventScreenDimming = "Evitar que se apague la pantalla"
    , reset = "Reiniciar"
    , add = "Agregar"
    , previous = "Anterior"
    , next = "Siguiente"
    , howToPlay = "Cómo Jugar"
    , gameOverview = "Elm Against Humanity es un juego de fiesta donde los jugadores combinan cartas para crear combinaciones divertidas o escandalosas."
    , playerRoles = "Roles de Jugador"
    , gameplaySteps = "Cómo Jugar"
    , czarRole = "Un jugador es el Zar de Cartas (este rol rota cada ronda)."
    , czarReadsCard = "El Zar de Cartas lee una carta negra en voz alta a todos los jugadores."
    , playersSelectCards = "Todos los demás jugadores seleccionan una carta blanca de su mano tocándola."
    , playersGivePhones = "Los jugadores entregan sus teléfonos al Zar de Cartas para que pueda ver todas las cartas blancas seleccionadas."
    , czarPicksBest = "El Zar de Cartas elige la mejor combinación de carta blanca en su opinión."
    , pointAwarded = "El jugador que envió la carta blanca elegida obtiene un punto."
    , newRoundStarts = "Una nueva ronda comienza con el siguiente jugador convirtiéndose en el Zar de Cartas."
    , noWhiteCardsLoaded = "No se cargaron cartas blancas"
    , noBlackCardsLoaded = "No se cargaron cartas negras"
    , error = "Ocurrió un error cargando el mazo."
    }


polishTranslations : Translations
polishTranslations =
    { settings = "Ustawienia"
    , whiteCards = "Białe Karty"
    , blackCards = "Czarne Karty"
    , help = "Pomoc"
    , selectLanguage = "Język"
    , preventScreenDimming = "Zapobiegaj wygaszaniu ekranu"
    , reset = "Resetuj"
    , add = "Dodaj"
    , previous = "Poprzedni"
    , next = "Następny"
    , howToPlay = "Jak Grać"
    , gameOverview = "Elm Against Humanity to gra imprezowa, w której gracze łączą karty, aby tworzyć zabawne lub skandaliczne kombinacje."
    , playerRoles = "Role Graczy"
    , gameplaySteps = "Jak Grać"
    , czarRole = "Jeden gracz to Car Kart (ta rola rotuje co rundę)."
    , czarReadsCard = "Car Kart czyta czarną kartę na głos wszystkim graczom."
    , playersSelectCards = "Wszyscy pozostali gracze wybierają jedną białą kartę z ręki, dotykając jej."
    , playersGivePhones = "Gracze przekazują swoje telefony Carowi Kart, aby mógł zobaczyć wszystkie wybrane białe karty."
    , czarPicksBest = "Car Kart wybiera najlepszą kombinację białej karty według swojej opinii."
    , pointAwarded = "Gracz, który wybrał wybrane białą kartę, otrzymuje jeden punkt."
    , newRoundStarts = "Nowa runda rozpoczyna się z kolejnym graczem stającym się Carem Kart."
    , noWhiteCardsLoaded = "Nie załadowano białych kart"
    , noBlackCardsLoaded = "Nie załadowano czarnych kart"
    , error = "Wystąpił błąd podczas ładowania talii."
    }


languageFromString : String -> Language
languageFromString str =
    case String.toLower (String.left 2 str) of
        "es" ->
            Spanish

        "pl" ->
            Polish

        _ ->
            English


languageToString : Language -> String
languageToString language =
    case language of
        English ->
            "en"

        Spanish ->
            "es"

        Polish ->
            "pl"
