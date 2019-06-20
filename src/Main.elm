module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (Html)
import Html.Attributes
import List.Nonempty as Nel exposing (Nonempty)
import Random
import Random.List



---- MODEL ----


type Operation
    = Addition
    | Subtraction
    | Multiplication


type Answer
    = Correct Int
    | Incorrect Int
    | NotAnswered


type alias Term =
    { operation : Operation
    , operandOne : Int
    , operandTwo : Int
    , result : Int
    }


type alias Model =
    { upper : Int
    , operations : Nonempty Operation
    , term : Term
    , choices : List Int
    , answer : Answer
    }


init : ( Model, Cmd Msg )
init =
    let
        upper =
            20

        operations =
            Nel.fromElement Addition
    in
    ( { upper = upper
      , operations = operations
      , term = { operation = Addition, operandOne = 1, operandTwo = 1, result = 2 }
      , choices = [ 1, 2, 3, 4, 5 ]
      , answer = NotAnswered
      }
    , generateQuestion operations upper
    )



---- UPDATE ----


generateQuestion : Nonempty Operation -> Int -> Cmd Msg
generateQuestion operations upper =
    rndTerm operations upper
        |> Random.andThen
            (\term ->
                rndChoices upper term.result |> Random.map (\xs -> ( term, xs ))
            )
        |> Random.generate QuestionResult


rndTerm : Nonempty Operation -> Int -> Random.Generator Term
rndTerm operations upper =
    Random.uniform (Nel.head operations) (Nel.tail operations)
        |> Random.andThen
            (\operation ->
                case operation of
                    Subtraction ->
                        Random.int 0 upper
                            |> Random.andThen
                                (\operandOne ->
                                    Random.int 0 operandOne
                                        |> Random.map
                                            (\operandTwo ->
                                                { operation = operation
                                                , operandOne = operandOne
                                                , operandTwo = operandTwo
                                                , result = operandOne - operandTwo
                                                }
                                            )
                                )

                    Addition ->
                        Random.int 0 upper
                            |> Random.andThen
                                (\operandOne ->
                                    Random.int 0 (upper - operandOne)
                                        |> Random.map
                                            (\operandTwo ->
                                                { operation = operation
                                                , operandOne = operandOne
                                                , operandTwo = operandTwo
                                                , result = operandOne + operandTwo
                                                }
                                            )
                                )

                    Multiplication ->
                        Random.int 0 upper
                            |> Random.andThen
                                (\operandOne ->
                                    Random.int 0 (upper // operandOne)
                                        |> Random.map
                                            (\operandTwo ->
                                                { operation = operation
                                                , operandOne = operandOne
                                                , operandTwo = operandTwo
                                                , result = operandOne * operandTwo
                                                }
                                            )
                                )
            )


rndChoices : Int -> Int -> Random.Generator (List Int)
rndChoices upper correctAnswer =
    Random.int 0 upper
        |> Random.list 4
        |> Random.map (List.append [ correctAnswer ])
        |> Random.andThen Random.List.shuffle


type Msg
    = QuestionResult ( Term, List Int )
    | Next
    | TrySolve Int
    | ToggleOperation Operation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionResult ( term, choices ) ->
            ( { model
                | term = term
                , choices = choices
                , answer = NotAnswered
              }
            , Cmd.none
            )

        Next ->
            ( model, generateQuestion model.operations model.upper )

        TrySolve n ->
            if n == model.term.result then
                ( { model | answer = Correct n }, Cmd.none )

            else
                ( { model | answer = Incorrect n }, Cmd.none )

        ToggleOperation op ->
            let
                newModel =
                    if model.operations |> Nel.member op then
                        { model | operations = Nel.filter ((==) op >> not) (Nel.head model.operations) model.operations }

                    else
                        { model | operations = Nel.cons op model.operations }
            in
            ( newModel, generateQuestion newModel.operations model.upper )



---- VIEW ----


viewTerm : Answer -> Term -> Html Msg
viewTerm answer { operation, operandOne, operandTwo } =
    let
        operator =
            case operation of
                Subtraction ->
                    " - "

                Addition ->
                    " + "

                Multiplication ->
                    " × "

        result =
            case answer of
                Correct n ->
                    " " ++ String.fromInt n ++ " " ++ String.fromChar '😄'

                Incorrect n ->
                    " " ++ String.fromInt n ++ " " ++ String.fromChar '🙈'

                NotAnswered ->
                    " _"
    in
    Html.text (String.fromInt operandOne ++ operator ++ String.fromInt operandTwo ++ " =" ++ result)


viewChoices : List Int -> List (Html Msg)
viewChoices =
    List.map
        (\x ->
            Button.button
                [ Button.dark
                , Button.attrs
                    [ Html.Attributes.style "font-size" "5vw"
                    , Html.Attributes.style "min-width" "10vw"
                    ]
                , Button.onClick (TrySolve x)
                ]
                [ Html.text (String.fromInt x) ]
        )


viewCheckbox : Model -> Html Msg
viewCheckbox model =
    let
        style =
            [ Html.Attributes.style "font-size" "5vw"
            ]
    in
    ButtonGroup.checkboxButtonGroup []
        [ ButtonGroup.checkboxButton
            (model.operations |> Nel.member Addition)
            [ Button.light
            , Button.onClick (ToggleOperation Addition)
            , Button.attrs style
            ]
            [ Html.i [ Html.Attributes.class "fas fa-plus" ] [] ]
        , ButtonGroup.checkboxButton
            (model.operations |> Nel.member Subtraction)
            [ Button.light
            , Button.onClick (ToggleOperation Subtraction)
            , Button.attrs style
            ]
            [ Html.i [ Html.Attributes.class "fas fa-minus" ] [] ]
        , ButtonGroup.checkboxButton
            (model.operations |> Nel.member Multiplication)
            [ Button.light
            , Button.onClick (ToggleOperation Multiplication)
            , Button.attrs style
            ]
            [ Html.i [ Html.Attributes.class "fas fa-times" ] [] ]
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ Html.div
                    [ Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "font-size" "10vw"
                    , Spacing.mt4
                    ]
                    [ viewTerm model.answer model.term ]
                , Html.div
                    [ Html.Attributes.style "font-size" "5vw"
                    , Flex.block
                    , Flex.row
                    , Flex.justifyBetween
                    ]
                    (viewChoices model.choices)
                , Html.div [ Size.w100, Flex.block, Flex.col, Flex.alignItemsCenter, Spacing.mt4 ]
                    [ Html.div [ Flex.block, Flex.row, Flex.justifyBetween, Size.w100 ]
                        [ viewCheckbox model
                        , Button.button
                            [ Button.light
                            , Button.onClick Next
                            , Button.attrs
                                [ Html.Attributes.style "font-size" "5vw"
                                , Html.Attributes.style "min-width" "10vw"
                                ]
                            ]
                            [ Html.i [ Html.Attributes.class "fas fa-step-forward" ] [] ]
                        ]
                    , Button.linkButton
                        [ Button.roleLink, Button.attrs [ Spacing.mt3, Html.Attributes.href "https://github.com/battermann/kids-math-quiz" ] ]
                        [ Html.i [ Html.Attributes.class "fab fa-github" ] [], Html.text " Source Code" ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
