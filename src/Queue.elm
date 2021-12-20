module Queue exposing (..)

import Dict exposing (Dict)


type alias Queue a =
    ( Dict Int a, ( Num, Num ) )


type Num
    = First Int
    | Last Int


toInt : Num -> Int
toInt num =
    case num of
        First int ->
            int

        Last int ->
            int


toFirst n =
    First n


toLast n =
    Last n


empty =
    ( Dict.empty, ( First 0, Last 0 ) )


firstIndex : Queue a -> Int
firstIndex que =
    que |> Tuple.second |> Tuple.first |> toInt


lastIndex : Queue a -> Int
lastIndex que =
    que |> Tuple.second |> Tuple.second |> toInt


id x =
    x


enq : a -> Queue a -> Queue a
enq a q =
    if Dict.isEmpty (Tuple.first q) then
        Tuple.mapBoth (Dict.insert 0 a) id q

    else
        ( Dict.insert (lastIndex q + 1) a (Tuple.first q), ( firstIndex q |> toFirst, (lastIndex q + 1) |> toLast ) )


deq : Queue a -> ( Maybe a, Queue a )
deq q =
    if (Tuple.first >> Dict.isEmpty) q then
        ( Nothing, q )

    else
        let
            newQ =
                Dict.remove (firstIndex q) (Tuple.first q)

            isReset =
                Dict.isEmpty newQ

            first =
                toFirst <|
                    if isReset then
                        0

                    else
                        firstIndex q + 1

            last =
                toLast <|
                    if isReset then
                        0

                    else
                        lastIndex q
        in
        ( Dict.get (firstIndex q) (Tuple.first q), ( newQ, ( first, last ) ) )


map : (Int -> a -> b) -> Queue a -> Queue b
map f q =
    ( q |> Tuple.first |> Dict.map f, q |> Tuple.second )


filter : (Int -> a -> Bool) -> Queue a -> Queue a
filter pred q =
    let
        newQDict =
            q |> Tuple.first |> Dict.filter pred

        keys =
            Dict.keys newQDict

        first =
            keys |> List.sort |> List.head |> Maybe.withDefault 0 |> toFirst

        last =
            keys |> List.sort |> List.reverse |> List.head |> Maybe.withDefault 0 |> toLast
    in
    ( newQDict, ( first, last ) )
