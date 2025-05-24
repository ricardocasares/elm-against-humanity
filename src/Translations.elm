module Translations exposing (Language(..), Translations, getTranslations, languageFromString, languageToString)


type Language
    = English
    | Spanish
    | Polish


type alias Translations =
    { -- Tab labels
      settings : String
    , scores : String
    , whiteCards : String
    , blackCards : String

    -- Settings page
    , selectLanguage : String
    , preventScreenDimming : String

    -- Scores page
    , reset : String
    , add : String

    -- Card navigation
    , previous : String
    , next : String

    -- Error messages
    , noWhiteCardsLoaded : String
    , noBlackCardsLoaded : String
    , error : String
    }


getTranslations : Language -> Translations
getTranslations language =
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
    , scores = "Scores"
    , whiteCards = "White Cards"
    , blackCards = "Black Cards"
    , selectLanguage = "Language"
    , preventScreenDimming = "Prevent screen dimming"
    , reset = "Reset"
    , add = "Add"
    , previous = "Previous"
    , next = "Next"
    , noWhiteCardsLoaded = "No white cards loaded"
    , noBlackCardsLoaded = "No black cards loaded"
    , error = "Error: "
    }


spanishTranslations : Translations
spanishTranslations =
    { settings = "Configuración"
    , scores = "Puntos"
    , whiteCards = "Cartas Blancas"
    , blackCards = "Cartas Negras"
    , selectLanguage = "Idioma"
    , preventScreenDimming = "Evitar que se apague la pantalla"
    , reset = "Reiniciar"
    , add = "Agregar"
    , previous = "Anterior"
    , next = "Siguiente"
    , noWhiteCardsLoaded = "No se cargaron cartas blancas"
    , noBlackCardsLoaded = "No se cargaron cartas negras"
    , error = "Error: "
    }


polishTranslations : Translations
polishTranslations =
    { settings = "Ustawienia"
    , scores = "Punkty"
    , whiteCards = "Białe Karty"
    , blackCards = "Czarne Karty"
    , selectLanguage = "Język"
    , preventScreenDimming = "Zapobiegaj wygaszaniu ekranu"
    , reset = "Resetuj"
    , add = "Dodaj"
    , previous = "Poprzedni"
    , next = "Następny"
    , noWhiteCardsLoaded = "Nie załadowano białych kart"
    , noBlackCardsLoaded = "Nie załadowano czarnych kart"
    , error = "Błąd: "
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
