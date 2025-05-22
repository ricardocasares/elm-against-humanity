module MainSpec exposing (suite)

import Expect
import Main exposing (DeckState(..), Model, Route(..), view)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, disabled, tag, text)
import ZipList as Zip


testModel : Model
testModel =
    { route = Blacks
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
                    |> Query.has [ text "Reiniciar" ]
        ]
