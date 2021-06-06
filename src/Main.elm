module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


-- MODEL

type alias Board = Array Int
type alias BlockSize = Int
type alias Sudoku = (Board, BlockSize)
type State = Success

type alias Model =
    { state : State
    , content : String
    , sudoku : Sudoku
    , backendUrl : String
    }

type alias SudokuResult
    = { answer : List Int
      , solved : Bool
      }

type Msg
    = Change String
    | UpdateBoard
    | Solve
    | GotResponse (Result Http.Error SudokuResult)

type alias Flag =
    { backendUrl : String
    }


initialSudokuContent
    = "000000000" ++
      "000059028" ++
      "006007130" ++
      "000000600" ++
      "500040002" ++
      "003000000" ++
      "054800700" ++
      "230410000" ++
      "000000000"

initialSudokuBlockSize = 3

initialSudoku =
    let board =
            initialSudokuContent
                |> getBoardList
                |> Array.fromList
    in
    ( board, initialSudokuBlockSize )

toDefaultInt : Int -> Char -> Int
toDefaultInt default char =
    case (String.toInt (String.fromChar char)) of
        Just i ->
            i
        Nothing ->
            default

getBoardList : String -> List Int
getBoardList boardText =
    List.map (toDefaultInt 0) (String.toList boardText)

initialModel : Model
initialModel =
    { state = Success
    , content = ""
    , sudoku = initialSudoku
    , backendUrl = "http://localhost:5000"
    }

--init : Flag -> (Model, Cmd Msg)
--init flag
--    = ( { initialModel | backendUrl = flag.backendUrl }
--      , Cmd.none
--      )
init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )


-- UPDATE

sudokuDecoder : Decode.Decoder SudokuResult
sudokuDecoder =
    Decode.succeed SudokuResult
        |> Pipeline.required "answer" (Decode.list Decode.int)
        |> Pipeline.required "solved" Decode.bool

solveSudokuRequest : Sudoku -> String -> List Int -> Cmd Msg
solveSudokuRequest sudoku backendUrl boardList =
    let body =
            Encode.object [("cellValues", Encode.list Encode.int boardList)
                          ,("blockSize", Encode.int (Tuple.second sudoku))
                          ]
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = backendUrl ++ "/sudoku/solve"
        , body = body
        , expect = Http.expectJson GotResponse sudokuDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change content ->
            ( { model | content = content }, Cmd.none )

        UpdateBoard ->
            -- Update sudoku board
            let newBoardArray =
                    model.content
                        |> Debug.log "Board Text"
                        |> getBoardList
                        |> Array.fromList
            in
            let newModel =
                    { model | sudoku = (newBoardArray, Tuple.second model.sudoku) }
            in
            ( newModel, Cmd.none )

        Solve ->
            -- Solve sudoku board
            let cmd =
                    Tuple.first model.sudoku
                        |> Array.toList
                        |> solveSudokuRequest model.sudoku model.backendUrl
            in
            ( model, cmd )

        GotResponse result ->
            -- Got backend response
            case result of
                Ok response ->
                    let _ = Debug.log "Ok" response in
                    if response.solved then
                         ( { model | sudoku
                               = ( Array.fromList response.answer
                                 , Tuple.second model.sudoku
                                 )
                         }, Cmd.none )
                    else
                        ( model, Cmd.none )

                Err err ->
                    let _ = Debug.log "Err" err in
                    ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


-- VIEW

getPx : Int -> String
getPx size =
    (String.fromInt size) ++ "px"

cellView : Maybe Int -> Int -> Html Msg
cellView value baseSize =
    let val =
            case value of
                Just v ->
                    String.fromInt v

                Nothing ->
                    "-"
    in
    let tdStyle =
            [ style "border" "1px solid #999999"
            , style "width" (getPx (baseSize // 9))
            , style "height" (getPx (baseSize // 9))
            , style "text-align" "center"
            , style "vertical-align" "middle"
            , style "padding" "0px"
            , style "font-size" (getPx (baseSize // 18))
            ]
    in
    td
        tdStyle
        [ text val ]

getCellIds : BlockSize -> Int -> List (List Int)
getCellIds blockSize blockId =
    {-
      Get cell IDs of the block
      (ex.)
        - 9x9 board (Block size is 3)
        - Block ID is 4 (Center of the board)

        -> [[30,31,32],[39,40,41],[48,49,50]]
    -}

    -- Upper left cellId in block
    let startIndex =
            (blockId // blockSize)* (blockSize ^ 3) +
            ((modBy blockSize blockId) * blockSize)
    in
    List.indexedMap
        (\i x -> List.map (\y -> y + (blockSize ^ 2) * i) x)
        (List.repeat blockSize <| List.range startIndex (startIndex + blockSize - 1))

blockView : Board -> BlockSize -> Int -> Int -> Html Msg
blockView board blockSize blockId baseSize =
    table
        []
        (List.map
             (\row ->
                  tr []
                  (List.map (\cell -> cellView (Array.get cell board) baseSize) row)
             )
             (getCellIds blockSize blockId)
        )

sudokuView : Sudoku -> Html Msg
sudokuView sudoku =
    let board = Tuple.first sudoku in
    let blockSize = Tuple.second sudoku in
    let blockIds =
            List.indexedMap
                (\i x -> List.map (\y -> y + blockSize * i) x)
                (List.repeat blockSize <| List.range 0 (blockSize - 1))
    in
    let baseSize = 630 in
    let tableStyle =
            [ style "table-layout" "fixed"
            --, style "border" "5px solid #000000"
            , style "width" (getPx baseSize)
            , style "height" (getPx baseSize)
            ]
    in
    let tdStyle =
            [ style "border" "1.5px solid #333333"
            , style "padding" "0px"
            , style "vertical-align" "top"
            , style "width" (getPx (baseSize // 3))
            , style "height" (getPx (baseSize // 3))
            ]
    in
    table
        tableStyle
        (List.map
             (\blockRow ->
                  tr []
                  (List.map
                       (\blockId -> td tdStyle [(blockView board blockSize blockId baseSize)])
                       blockRow
                  )
             )
             blockIds
        )

isValidContent : String -> Bool
isValidContent content =
    String.length content
        |> Debug.log "content length: "
        |> (==) 81

view : Model -> Html Msg
view model =
    case model.state of
        Success ->
            div []
                [ sudokuView model.sudoku
                , input
                      [ placeholder "Input Sudoku Board!"
                      , value model.content
                      , onInput Change
                      ]
                      []
                , button
                      [ onClick UpdateBoard
                      , disabled (not (isValidContent model.content))
                      ]
                      [ text "更新" ]
                , button
                      [ onClick Solve
                      ]
                      [ text "解く" ]
                ]


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
