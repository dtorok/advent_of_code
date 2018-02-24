module Test exposing (Test, test)

import List
import Html exposing (..)
import Html.Attributes exposing (style)

type alias Test =
  { title: String
  , solver: String -> String
  , testCases: List (String, String)
  }

-- test : String -> (String -> String) -> List (String, String) -> Html a
-- test title f data =
test : Test -> Html a
test t =
  let
    solve : (String, String) -> (String, String)
    solve (input, expected) = (t.solver input, expected)

    results : List (String, String)
    results = List.map solve t.testCases

    getAssessment : String -> String -> Html a
    getAssessment input expected =
      if input == expected then
        span [style [("backgroundColor", "green")]] [text "[SUCCESS]"]
      else
        span [style [("backgroundColor", "red")]] [text "[FAIL]"]

    createLi : (String, String) -> Html a
    createLi (result, expected) =
      li []
        [ text result
        , text " =?= "
        , text expected
        , text "  ->  "
        , getAssessment result expected]

  in
    div []
      [ h2 [] [ text t.title ]
      , ul [] (List.map createLi results)
      ]
