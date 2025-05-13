module MainSpec exposing (suite)

import Main exposing (view)
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector as Html


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "Displays the current prompt" <|
            \_ ->
                view { prompt = "something funny" }
                    |> Query.fromHtml
                    |> Query.has [ Html.text "something funny" ]
        ]
