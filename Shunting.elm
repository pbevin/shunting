import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Char
import List exposing (..)

import StartApp.Simple


type alias Model =
  { input : String
  , opstack : List String
  , output : List String
  }

type Action = Input String
            | Clear


-- model

initModel =
  { input = ""
  , opstack = []
  , output = []
  }

-- update

update : Action -> Model -> Model
update action model =
  case action of
    Input expr ->
      let (opstack, output) = parse expr
      in  { input = expr
          , opstack = opstack
          , output = output
          }
    Clear -> initModel


type alias Arg = Float
type alias Precedence = Int
type alias Operator = String
type Token = Op Operator | Num Arg

prec : Operator -> Precedence
prec op = case op of
  "+" -> 1
  "-" -> 1
  "*" -> 2
  "/" -> 2
  otherwise -> -1

toArg : String -> Arg
toArg s = case String.toFloat s of
  Ok a -> a
  Err err -> 0

binary : Operator -> Arg -> Arg -> Arg
binary op e1 e2 =
  case op of
    "+" -> e1 + e2
    "-" -> e1 - e2
    "*" -> e1 * e2
    "/" -> e1 / e2
    otherwise -> 0

parse : String -> (List String, List String)
parse input =
  let tokens = input |> String.toList |> tokenize
      step tok (opstack, output) =
        case tok of
          Num num -> (opstack, num :: output)
          Op op   ->
            case opstack of
              [] -> (op :: opstack, output)
              op2::rest -> 
                if | prec op <= prec op2 -> step tok (rest, apply op2 output)
                   | otherwise -> (op :: opstack, output)
      apply op output =
        case output of
          e2 :: e1 :: rest -> binary op e1 e2 :: rest
          otherwise        -> []

      (opstack, output) = foldl step ([], []) tokens
  in (opstack, map toString output)

tokenize : List Char -> List Token
tokenize xs =
  case xs of
    []       -> []
    ch::rest ->
      if | ch == ' '       -> tokenize rest
         | Char.isDigit ch -> let (n, tail) = readWhile Char.isDigit (ch :: rest)
                              in Num (n |> String.fromList |> toArg) :: tokenize tail
         | otherwise       -> Op (String.fromChar ch) :: tokenize rest

readWhile : (a -> Bool) -> List a -> (List a, List a)
readWhile p xs =
  case xs of
    []   -> ([], [])
    x::xs ->
      if p x
         then let (us,vs) = readWhile p xs in (x::us, vs)
         else ([], x::xs)
    

-- view

view : Signal.Address Action -> Model -> Html
view address model =
  div [appStyle]
    [ h1 [titleStyle] [Html.text "Shunting Yard"]
    , input
        [ placeholder "Enter an expression"
        , value model.input
        , on "input" targetValue (Signal.message address << Input)
        , inputStyle
        ]
        []
    , output "Operator Stack" <| String.join ", " <| model.opstack
    , output "Output Stack" <| String.join ", " <| model.output
    ]

output : String -> String -> Html
output labelText value =
  div [outputStyle]
    [ label [labelStyle]
      [ Html.text labelText
      , Html.output [consoleStyle] [ Html.text value ]
      ]
    ]

appStyle : Attribute
appStyle =
  style
    [ ("background", "#333")
    , ("color", "#fff")
    , ("padding", "50px 20px")
    , ("min-height", "1000px") 
    ]

titleStyle = style [ ("text-align", "center") ]

inputStyle : Attribute
inputStyle =
  style
    [ ("display", "block")
    , ("font-size", "16px")
    , ("margin", "40px auto")
    , ("width", "400px")
    ]


outputStyle : Attribute
outputStyle =
  style
    [ ("width", "400px")
    , ("margin", "0 auto 10px")
    ]

labelStyle : Attribute
labelStyle =
  style
    [ ("display", "block")
    , ("margin-top", "30px")
    ]

consoleStyle : Attribute
consoleStyle =
  style
    [ ("font-family", "\"Lucida Console\", Monaco, monospace")
    , ("font-size", "14px")
    , ("background", "#d2d4d6")
    , ("color", "#333")
    , ("display", "block")
    , ("width", "100%")
    ]


main = StartApp.Simple.start { model = initModel, update = update, view = view }

