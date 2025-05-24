module MainSpec exposing (suite)

import Expect
import Main exposing (DeckState(..), Model, Route(..), WakeLockStatus(..), view)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, disabled, tag, text)
import Translations exposing (Language(..))
import ZipList as Zip


testModel : Model
testModel =
    { route = Blacks
    , state = Loading
    , deck = Nothing
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
    , currentLanguage = English
    }


suite : Test
suite =
    describe "Main"
        [ test "Shows loading state initially" <|
            \_ ->
                view testModel
                    |> Query.fromHtml
                    |> Query.has [ class "skeleton" ]
        , test "Initially has two players" <|
            \_ ->
                view { testModel | route = Scores, state = Loaded }
                    |> Query.fromHtml
                    |> Query.findAll [ tag "input" ]
                    |> Query.count (Expect.equal 2)
        , test "Remove buttons are disabled with only two players" <|
            \_ ->
                view { testModel | route = Scores, state = Loaded }
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button", class "btn-error" ]
                    |> Query.first
                    |> Query.has [ disabled True ]
        , test "Shows black card when present" <|
            \_ ->
                view
                    { testModel
                        | route = Blacks
                        , state = Loaded
                        , blacks = Just (Zip.new "A black card" [])
                        , whites = Just (Zip.new "" [])
                    }
                    |> Query.fromHtml
                    |> Query.has [ text "A black card" ]
        , test "Shows reset scores button" <|
            \_ ->
                view
                    { testModel
                        | route = Scores
                        , state = Loaded
                        , players =
                            [ { id = 1
                              , score = 5
                              , color = "bg-red-500"
                              }
                            , { id = 2
                              , score = 3
                              , color = "bg-blue-500"
                              }
                            ]
                    }
                    |> Query.fromHtml
                    |> Query.has [ text "Reset" ]
        , test "Wake lock button is disabled when status is unknown" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockUnknown }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled True, class "toggle-info" ]
        , test "Wake lock button is enabled when supported" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockSupported }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled False, class "toggle" ]
        , test "Wake lock button shows error state when not supported" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockNotSupported }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled True, class "toggle-error" ]
        , test "Wake lock button is checked when active" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockActive }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled False, class "toggle-success" ]
        , test "Wake lock button is unchecked when inactive" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockInactive }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled False, class "toggle-primary" ]
        , test "Wake lock button shows error when failed" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded, wakeLockStatus = WakeLockFailed "Error message" }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", class "toggle" ]
                    |> Query.has [ disabled True, class "toggle-error" ]
        , test "Settings view contains wake lock button" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded }
                    |> Query.fromHtml
                    |> Query.has [ tag "input", class "toggle" ]
        , test "Settings view shows prevent screen dimming label" <|
            \_ ->
                view { testModel | route = Settings, state = Loaded }
                    |> Query.fromHtml
                    |> Query.has [ text "Prevent screen dimming" ]
        ]
