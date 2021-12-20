module MaxHeap exposing (..)

import Array exposing (Array)
import Array.Extra as Array


type alias MaxBinaryHeap comparable =
    Array comparable


empty =
    Array.empty


insert : comparable -> MaxBinaryHeap comparable -> MaxBinaryHeap comparable
insert a heap =
    if Array.any ((==) a) heap then
        heap

    else
        let
            newHeap =
                Array.push a heap
        in
        rebalanceHeapForInsert a (Array.length newHeap - 1) newHeap


rebalanceHeapForInsert : comparable -> Int -> MaxBinaryHeap comparable -> MaxBinaryHeap comparable
rebalanceHeapForInsert val currIndex heap =
    if currIndex == 0 then
        heap

    else
        let
            parentIndex =
                (currIndex - 1) // 2

            parentVal =
                Array.get parentIndex heap

            needsRebalancing =
                Maybe.map (\p -> p < val) parentVal |> Maybe.withDefault False

            swappedHeap =
                if needsRebalancing then
                    heap
                        |> Array.set parentIndex val
                        |> (\a -> Maybe.map2 (\arr pval -> Array.set currIndex pval arr) (Just a) parentVal |> Maybe.withDefault a)

                else
                    heap
        in
        if not needsRebalancing then
            swappedHeap

        else
            rebalanceHeapForInsert val parentIndex swappedHeap


exampleHeap =
    Array.fromList
        [ 40
        , 30
        , 20
        , 19
        , 18
        , 16
        , 15
        ]


extractMax : MaxBinaryHeap comparable -> ( Maybe comparable, MaxBinaryHeap comparable )
extractMax heap =
    let
        lastVal =
            Array.get (Array.length heap - 1) heap

        initHeap =
            heap |> set_ 0 lastVal |> Array.removeAt (Array.length heap - 1)
    in
    ( Array.get 0 heap, rebalanceHeapForExtractMax initHeap )


rebalanceHeapForExtractMax : MaxBinaryHeap comparable -> MaxBinaryHeap comparable
rebalanceHeapForExtractMax heap =
    let
        go : Int -> MaxBinaryHeap comparable -> MaxBinaryHeap comparable
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
                    Maybe.map2 (\a b -> a < b) val child1 |> Maybe.withDefault False

                lessThanChild2 =
                    Maybe.map2 (\a b -> a < b) val child2 |> Maybe.withDefault False

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
