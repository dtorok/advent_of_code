module TestDay21 exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, test, describe)

import Day21

suite : Test
suite =
  let
    m4x4 =
      [ [1, 2, 3, 4]
      , [2, 3, 4, 5]
      , [3, 4, 5, 6]
      , [4, 5, 6, 7] ]

    m4x4split2 =
      [ [ [ [1, 2]
          , [2, 3] ]
        , [ [3, 4]
          , [4, 5] ] ]
        , [ [ [3, 4]
          , [4, 5] ]
        , [ [5, 6]
          , [6, 7] ] ] ]

    m6x6 =
      [ [1, 2, 3, 4, 5, 6]
      , [2, 3, 4, 5, 6, 7]
      , [3, 4, 5, 6, 7, 8]
      , [4, 5, 6, 7, 8, 9]
      , [5, 6, 7, 8, 9, 0]
      , [1, 3, 5, 7, 9, 0] ]

    m6x6split3 =
      [ [ [ [1, 2, 3]
          , [2, 3, 4]
          , [3, 4, 5] ]
        , [ [4, 5, 6]
          , [5, 6, 7]
          , [6, 7, 8] ] ]
      , [ [ [4, 5, 6]
          , [5, 6, 7]
          , [1, 3, 5] ]
        , [ [7, 8, 9]
          , [8, 9, 0]
          , [7, 9, 0] ] ] ]

  in
    describe "Day21"
        [ describe "transpose"
            [ test "test 2x2" <|
              \_ ->
                let
                    input = [[1, 2], [3, 4]]
                    expected = [[1, 3], [2, 4]]
                in
                    Expect.equal expected (Day21.transpose input)
            , test "test 3x3" <|
              \_ ->
                let
                    input = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
                    expected = [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
                in
                    Expect.equal expected (Day21.transpose input)
            ], test "test non-square" <|
              \_ ->
                let
                  input =
                    [ [ [1, 2, 3]
                      , [2, 3, 4]
                      , [3, 4, 5] ]
                    , [ [4, 5, 6]
                      , [5, 6, 7]
                      , [6, 7, 8] ] ]

                  expected =
                    [ [ [1, 2, 3], [4, 5, 6] ]
                    , [ [2, 3, 4], [5, 6, 7] ]
                    , [ [3, 4, 5], [6, 7, 8] ] ]
                in
                    Expect.equal expected (Day21.transpose input)


        , describe "split"
            [ test "split to 2x2" <|
                \_ ->
                    let
                        input = m4x4
                        expected = m4x4split2
                    in
                        Expect.equal expected (Day21.split 2 input)
            , test "split to 3x3" <|
                \_ ->
                    let
                        input = m6x6
                        expected = m6x6split3
                    in
                        Expect.equal expected (Day21.split 3 input)
            ]
        , describe "join"
            [ test "join 2x2" <|
                \_ ->
                    let
                        input = m4x4split2
                        expected = m4x4
                    in
                        Expect.equal expected (Day21.join input)
            , test "join 3x3" <|
                \_ ->
                    let
                        input = m6x6split3
                        expected = m6x6
                    in
                      Expect.equal expected (Day21.join input)
            , describe "helpers"
              [ test "rotateCcw" <|
                  \_ ->
                      let
                        input =
                          [ [ 1, 2, 3 ]
                          , [ 2, 3, 4 ]
                          , [ 3, 4, 5 ] ]
                        expected =
                          [ [ 3, 4, 5 ]
                          , [ 2, 3, 4 ]
                          , [ 1, 2, 3 ] ]
                      in
                        Expect.equal expected (Day21.rotateCcw input)
              , test "flipV" <|
                  \_ ->
                    let
                      input =
                        [ [ 1, 2, 3, 4 ]
                        , [ 2, 3, 4, 5 ]
                        , [ 3, 4, 5, 6 ]
                        , [ 4, 5, 6, 7 ] ]
                      expected =
                        [ [ 4, 5, 6, 7 ]
                        , [ 3, 4, 5, 6 ]
                        , [ 2, 3, 4, 5 ]
                        , [ 1, 2, 3, 4 ] ]
                    in
                      Expect.equal expected (Day21.flipV input)
              , test "flipH" <|
                  \_ ->
                    let
                      input =
                        [ [ 1, 2, 3, 4 ]
                        , [ 2, 3, 4, 5 ]
                        , [ 3, 4, 5, 6 ]
                        , [ 4, 5, 6, 7 ] ]
                      expected =
                        [ [ 4, 3, 2, 1 ]
                        , [ 5, 4, 3, 2 ]
                        , [ 6, 5, 4, 3 ]
                        , [ 7, 6, 5, 4 ] ]
                    in
                      Expect.equal expected (Day21.flipH input)
              ]

            ]
        ]
