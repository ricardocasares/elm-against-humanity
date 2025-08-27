module Translations exposing (Language(..), Translations, english, fromString, toString, translate)


type Language
    = English
    | Spanish
    | Polish
    | Chinese
    | Italian


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
            english

        Spanish ->
            spanish

        Polish ->
            polish

        Chinese ->
            chinese

        Italian ->
            italian


english : Translations
english =
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


spanish : Translations
spanish =
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


polish : Translations
polish =
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


chinese : Translations
chinese =
    { settings = "设置"
    , whiteCards = "白卡"
    , blackCards = "黑卡"
    , help = "帮助"
    , selectLanguage = "语言"
    , preventScreenDimming = "防止屏幕变暗"
    , reset = "重置"
    , add = "添加"
    , previous = "上一个"
    , next = "下一个"
    , howToPlay = "游戏方法"
    , gameOverview = "Elm Against Humanity 是一个聚会游戏，玩家通过组合卡片来创造有趣或令人发指的组合。"
    , playerRoles = "玩家角色"
    , gameplaySteps = "游戏方法"
    , czarRole = "一名玩家担任卡片皇帝（此角色每轮轮换）。"
    , czarReadsCard = "卡片皇帝向所有玩家大声读出一张黑卡。"
    , playersSelectCards = "所有其他玩家通过点击从手中选择一张白卡。"
    , playersGivePhones = "玩家将手机交给卡片皇帝，以便他们可以看到所有选中的白卡。"
    , czarPicksBest = "卡片皇帝根据自己的意见选择最佳的白卡组合。"
    , pointAwarded = "提交被选中白卡的玩家获得一分。"
    , newRoundStarts = "新一轮开始，下一名玩家成为卡片皇帝。"
    , noWhiteCardsLoaded = "未加载白卡"
    , noBlackCardsLoaded = "未加载黑卡"
    , error = "加载牌组时发生错误。"
    }


italian : Translations
italian =
    { settings = "Impostazioni"
    , whiteCards = "Carte Bianche"
    , blackCards = "Carte Nere"
    , help = "Aiuto"
    , selectLanguage = "Lingua"
    , preventScreenDimming = "Impedisci il buio dello schermo"
    , reset = "Ripristina"
    , add = "Aggiungi"
    , previous = "Precedente"
    , next = "Successivo"
    , howToPlay = "Come Giocare"
    , gameOverview = "Elm Against Humanity è un gioco di festa dove i giocatori combinano carte per creare combinazioni divertenti o scandalose."
    , playerRoles = "Ruoli dei Giocatori"
    , gameplaySteps = "Come Giocare"
    , czarRole = "Un giocatore è il Czar delle Carte (questo ruolo cambia ogni round)."
    , czarReadsCard = "Il Czar delle Carte legge ad alta voce una carta nera a tutti i giocatori."
    , playersSelectCards = "Tutti gli altri giocatori selezionano una carta bianca dalla propria mano toccandola."
    , playersGivePhones = "I giocatori consegnano i loro telefoni al Czar delle Carte in modo che possano vedere tutte le carte bianche selezionate."
    , czarPicksBest = "Il Czar delle Carte sceglie la miglior combinazione di carte bianche secondo la sua opinione."
    , pointAwarded = "Il giocatore che ha inviato la carta bianca scelta ottiene un punto."
    , newRoundStarts = "Un nuovo round inizia con il prossimo giocatore che diventa il Czar delle Carte."
    , noWhiteCardsLoaded = "Nessuna carta bianca caricata"
    , noBlackCardsLoaded = "Nessuna carta nera caricata"
    , error = "Si è verificato un errore durante il caricamento del mazzo."
    }


fromString : String -> Language
fromString str =
    case String.toLower (String.left 2 str) of
        "es" ->
            Spanish

        "pl" ->
            Polish

        "zh" ->
            Chinese

        "it" ->
            Italian

        _ ->
            English


toString : Language -> String
toString language =
    case language of
        English ->
            "en"

        Spanish ->
            "es"

        Polish ->
            "pl"

        Chinese ->
            "zh"

        Italian ->
            "it"
