module Main exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text, program)
import Json.Decode as Json
import List

transitionDuration =
  1000


defaultModel =
  { shuffle = False
  , list = Nothing
  , time = 0
  }


type Msg
  = Shuffle
  | Reconfigure ChildListReconfigure
  | Tick Float


main =
  program
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }


init =
  ( defaultModel, Cmd.none )


subscriptions model =
  if model.time > 0 then
    AnimationFrame.diffs Tick
  else
    Sub.none


update msg model =
  case msg of
    Shuffle ->
      ( { model | shuffle = not model.shuffle }, Cmd.none )

    Reconfigure childListReconfigure ->
      ( { model
          | list = Just childListReconfigure
          , time = transitionDuration
        }
      ,
        Cmd.none
      )

    Tick dt ->
      let
        time =
          max 0 (model.time - dt)
      in
      ( { model
          | time = time
          , list =
              if time > 0 then
                model.list
              else
                Nothing
        }
      ,
        Cmd.none
      )


view model =
  Html.div
  [
  ]
  [ Html.button
    [ Html.onClick Shuffle
    ]
    [ text "Shuffle"
    ]
  , list (clamp 0 1 (model.time / transitionDuration)) model.list
      (
        ( if model.shuffle then
            [ 1, 0, 5, 6, 3, 2, 8, 9, 4, 7
            ]
          else
            List.range 0 9
        )
        |> List.map (\ n -> ( toString n, toString n ))
      )
  ]


-- list component


list : Float -> Maybe ChildListReconfigure -> List ( String, String ) -> Html Msg
list t childListReconfigure items =
  Html.ul
  [ Html.on "my-child-list-reconfigure" (Json.map Reconfigure myChildListReconfigure)
  ]
  ( items
    |> List.map (\ ( id, label ) ->
         let
           transform =
             childListReconfigure
             |> Maybe.map (\ { changes, bounds } ->
                 let
                   currentBounds =
                     Dict.get id bounds
                     |> Maybe.withDefault nullRect

                   lastBounds =
                     Dict.get lastId bounds
                     |> Maybe.withDefault nullRect

                   lastId =
                     changes
                     |> List.filterMap (\ ( new, old ) ->
                          if id == new then
                            Just old
                          else
                            Nothing
                        )
                     |> List.head
                     |> Maybe.withDefault id

                   y =
                     t * (lastBounds.top - currentBounds.top)
                     |> Debug.log id
                 in
                 "translate3d(0," ++ toString y ++ "px,0)"
                )
             |> Maybe.withDefault "translate3d(0,0,0)"
         in
         Html.li
         [ Html.attribute "data-flip-id" id
         , Html.style
           [ ( "transform", transform )
           , ( "postition", "relative" )
           ]
         ]
         [ text label
         ]
       )
  )


type alias ChildListReconfigure =
  { bounds : Dict String Rect
  , changes : List ( String, String )
  }


type alias Rect =
  { top : Float
  , left : Float
  , bottom : Float
  , right: Float
  }


nullRect =
  { top = 0, left = 0, bottom = 0, right = 0 }


myChildListReconfigure =
  let
    bounds =
      Json.map Dict.fromList <|
      Json.at ["bounds"] <|
      Json.list <|
      Json.map2 (,)
        (Json.at ["id"] Json.string)
        (Json.at ["rect"] rect)

    changes =
      Json.at ["changes"] <|
      Json.list <|
      ( Json.list Json.string
        |> Json.andThen (\ xs ->
             case xs of
               [ x, y ] ->
                 Json.succeed ( x, y )
               _ ->
                 Json.fail "Main: unexpected changes"
           )
      )

    rect =
      Json.map4 Rect
        (Json.at ["top"] Json.float)
        (Json.at ["left"] Json.float)
        (Json.at ["bottom"] Json.float)
        (Json.at ["right"] Json.float)
  in
  Json.map2 ChildListReconfigure bounds changes
