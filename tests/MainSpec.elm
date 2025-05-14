module MainSpec exposing (suite)

import Main exposing (Route(..), view)
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector as Html


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "Displays the current deck" <|
            \_ ->
                view { route = Blacks, blacks = [ "A black card" ], whites = [] }
                    |> Query.fromHtml
                    |> Query.has [ Html.text "A black card" ]
        ]
