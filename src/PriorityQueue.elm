module PriorityQueue exposing (..)

import Array exposing (Array)
import Array.Extra as Array


type alias Node a =
    ( Int, a )


type alias PriorityQueue a =
    Array (Node a)


empty =
    Array.empty


enq : Node a -> PriorityQueue a -> PriorityQueue a
enq ( priority, a ) heap =
    if Array.any (Tuple.second >> (==) a) heap then
        heap

    else
        let
            newHeap =
                Array.push ( priority, a ) heap
        in
        rebalanceHeapForInsert ( priority, a ) (Array.length newHeap - 1) newHeap


rebalanceHeapForInsert : Node a -> Int -> PriorityQueue a -> PriorityQueue a
rebalanceHeapForInsert ( priority, a ) currIndex heap =
    if currIndex == 0 then
        heap

    else
        let
            parentIndex =
                (currIndex - 1) // 2

            parentVal =
                Array.get parentIndex heap

            needsRebalancing =
                Maybe.map (\p -> Tuple.first p < priority) parentVal |> Maybe.withDefault False

            swappedHeap =
                if needsRebalancing then
                    heap
                        |> Array.set parentIndex ( priority, a )
                        |> (\h -> Maybe.map2 (Array.set currIndex) parentVal (Just h) |> Maybe.withDefault h)

                else
                    heap
        in
        if not needsRebalancing then
            swappedHeap

        else
            rebalanceHeapForInsert ( priority, a ) parentIndex swappedHeap


exampleHeap : PriorityQueue String
exampleHeap =
    Array.fromList
        [ ( 40, "hello" )
        , ( 30, "there" )
        , ( 20, "general" )
        , ( 19, "kenobi" )
        , ( 18, "What" )
        , ( 16, "do" )
        , ( 15, "you" )
        ]


deq : PriorityQueue a -> ( Maybe a, PriorityQueue a )
deq heap =
    let
        lastVal =
            Array.get (Array.length heap - 1) heap

        initHeap =
            heap |> set_ 0 lastVal |> Array.removeAt (Array.length heap - 1)
    in
    ( (Array.get 0 >> Maybe.map Tuple.second) heap, rebalanceHeapForExtractMax initHeap )


rebalanceHeapForExtractMax : PriorityQueue a -> PriorityQueue a
rebalanceHeapForExtractMax heap =
    let
        go : Int -> PriorityQueue a -> PriorityQueue a
        go index heap_ =
            let
                val =
                    Array.get index heap_

                child1Index =
                    2 * index + 1

                child2Index =
                    2 * index + 2

                ( child1, child2 ) =
                    Tuple.mapBoth (Array.get child1Index) (Array.get child2Index) ( heap_, heap_ )

                lessThanChild1 =
                    Maybe.map2 (\a b -> Tuple.first a < Tuple.first b) val child1 |> Maybe.withDefault False

                lessThanChild2 =
                    Maybe.map2 (\a b -> Tuple.first a < Tuple.first b) val child2 |> Maybe.withDefault False

                ( swappedHeap, newIndex ) =
                    if lessThanChild1 then
                        Tuple.pair
                            (heap_
                                |> set_ child1Index val
                                |> set_ index child1
                            )
                            child1Index

                    else if lessThanChild2 then
                        Tuple.pair
                            (heap_
                                |> set_ child2Index val
                                |> set_ index child2
                            )
                            child2Index

                    else
                        Tuple.pair heap_ -1
            in
            if not lessThanChild1 && not lessThanChild1 then
                swappedHeap

            else
                go newIndex swappedHeap
    in
    go 0 heap


set_ : Int -> Maybe a -> Array a -> Array a
set_ index maybeVal arr =
    Maybe.map (\v -> Array.set index v arr) maybeVal |> Maybe.withDefault arr
