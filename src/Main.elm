module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Random
import Random.List



---- MODEL ----


type Operation
    = Add


type Answer
    = Correct Int
    | Incorrect Int
    | None


type alias Model =
    { upper : Int
    , operation : Operation
    , numbers : Maybe ( Int, Int )
    , solutions : List Int
    , correctSolution : Maybe Int
    , result : Answer
    }


init : ( Model, Cmd Msg )
init =
    ( { upper = 20, operation = Add, numbers = Nothing, solutions = [], result = None, correctSolution = Nothing }, generateQuestion Add 20 )



---- UPDATE ----


generateQuestion : Operation -> Int -> Cmd Msg
generateQuestion operation upper =
    rndNumbers operation upper
        |> Random.andThen (\( nums, solution ) -> solutions operation nums |> Random.map (\xs -> ( nums, solution, xs )))
        |> Random.generate QuestionResult


rndNumbers : Operation -> Int -> Random.Generator ( ( Int, Int ), Int )
rndNumbers operation upper =
    case operation of
        Add ->
            Random.int 0 upper
                |> Random.andThen
                    (\first ->
                        Random.int 0 (upper - first)
                            |> Random.map (\second -> ( ( first, second ), first + second ))
                    )


solutions : Operation -> ( Int, Int ) -> Random.Generator (List Int)
solutions operation ( x, y ) =
    case operation of
        Add ->
            Random.int 0 20
                |> Random.list 3
                |> Random.map (List.append [ x + y ])
                |> Random.andThen Random.List.shuffle


type Msg
    = QuestionResult ( ( Int, Int ), Int, List Int )
    | Next
    | TrySolve Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionResult ( nums, solution, xs ) ->
            ( { model | numbers = Just nums, solutions = xs, correctSolution = Just solution, result = None }, Cmd.none )

        Next ->
            ( model, generateQuestion model.operation model.upper )

        TrySolve n ->
            case model.operation of
                Add ->
                    if model.correctSolution |> Maybe.Extra.toList |> List.member n then
                        ( { model | result = Correct n }, Cmd.none )

                    else
                        ( { model | result = Incorrect n }, Cmd.none )



---- VIEW ----


viewTerm : Answer -> Operation -> ( Int, Int ) -> Html Msg
viewTerm answer operation ( x, y ) =
    let
        operator =
            case operation of
                Add ->
                    " + "

        result =
            case answer of
                Correct n ->
                    " " ++ String.fromInt n ++ " " ++ String.fromChar '😄'

                Incorrect n ->
                    " " ++ String.fromInt n ++ " " ++ String.fromChar'🙈'

                None ->
                    " _"
    in
    Html.text (String.fromInt x ++ operator ++ String.fromInt y ++ " =" ++ result)


viewSolutions : List Int -> List (Html Msg)
viewSolutions xs =
    xs
        |> List.map
            (\x ->
                Button.button
                    [ Button.light
                    , Button.attrs [ Html.Attributes.style "font-size" "5vw" ]
                    , Button.onClick (TrySolve x)
                    ]
                    [ Html.text (String.fromInt x) ]
            )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ Html.div
                    [ Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "font-size" "10vw"
                    ]
                    [ model.numbers |> Maybe.map (viewTerm model.result model.operation) |> Maybe.withDefault (Html.text "") ]
                , Html.div
                    [ Html.Attributes.style "font-size" "5vw"
                    , Flex.block
                    , Flex.row
                    , Flex.justifyAround
                    ]
                    (viewSolutions model.solutions)
                , Html.div [ Size.w100, Flex.block, Flex.row, Flex.justifyAround, Spacing.mt3 ]
                    [ Button.button [ Button.outlinePrimary, Button.onClick Next ] [ Html.text "Next" ] ]
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
