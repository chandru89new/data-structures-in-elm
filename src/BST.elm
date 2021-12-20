module BST exposing (..)

import Queue as Q
import Stack exposing (Stack)


type BST comparable
    = Node comparable (BST comparable) (BST comparable)
    | Empty


empty =
    Empty


add : comparable -> BST comparable -> BST comparable
add a bst =
    case bst of
        Node s left right ->
            if s > a then
                Node s (add a left) right

            else if s < a then
                Node s left (add a right)

            else
                Node s left right

        Empty ->
            Node a empty empty


find : comparable -> BST comparable -> BST comparable
find a bst =
    case bst of
        Empty ->
            empty

        Node s left right ->
            if s == a then
                Node a left right

            else if s < a then
                find a right

            else
                find a left


max : BST comparable -> Maybe comparable
max bst =
    case bst of
        Node s _ right ->
            case right of
                Empty ->
                    Just s

                _ ->
                    max right

        Empty ->
            Nothing


bfs : BST comparable -> List comparable
bfs bst =
    case bst of
        Empty ->
            []

        Node _ _ _ ->
            let
                go que acc =
                    if que == Q.empty then
                        acc

                    else
                        let
                            ( item, restOfQue ) =
                                Q.deq que

                            root =
                                Maybe.andThen
                                    (\tree ->
                                        case tree of
                                            Empty ->
                                                Nothing

                                            Node a_ _ _ ->
                                                Just a_
                                    )
                                    item

                            acc_ =
                                case root of
                                    Nothing ->
                                        acc

                                    Just a_ ->
                                        a_ :: acc

                            que_ =
                                case item of
                                    Nothing ->
                                        restOfQue

                                    Just tree ->
                                        case tree of
                                            Empty ->
                                                restOfQue

                                            Node _ left right ->
                                                let
                                                    leftEnqued =
                                                        if left == empty then
                                                            restOfQue

                                                        else
                                                            Q.enq left restOfQue

                                                    rightEnqued =
                                                        if right == empty then
                                                            leftEnqued

                                                        else
                                                            Q.enq right leftEnqued
                                                in
                                                rightEnqued
                        in
                        go que_ acc_
            in
            go (Q.enq bst Q.empty) [] |> List.reverse


exampleTree =
    [ 2, 10, 15, 198, 276, 100, 41, 75, 33, 398, 23, 66, 89 ] |> fromList


fromList : List comparable -> BST comparable
fromList list =
    List.foldl add empty list


remove : comparable -> BST comparable -> BST comparable
remove a tree =
    case tree of
        Empty ->
            empty

        Node v left right ->
            if v == a then
                if right == empty then
                    left

                else
                    let
                        minVal =
                            min right
                    in
                    case minVal of
                        Just val ->
                            Node val left (remove val right)

                        Nothing ->
                            left

            else
                Node v (remove a left) (remove a right)


min : BST comparable -> Maybe comparable
min tree =
    case tree of
        Empty ->
            Nothing

        Node a left _ ->
            if left == empty then
                Just a

            else
                min left


dfsInOrder : BST comparable -> List comparable
dfsInOrder tree =
    let
        go : Stack (BST comparable) -> List comparable -> List comparable
        go stack acc =
            if stack == Stack.empty then
                acc

            else
                let
                    ( maybeNode, restOfTheStack ) =
                        Stack.pop stack

                    node =
                        Maybe.withDefault Empty maybeNode

                    ( acc_, newStack ) =
                        case node of
                            Empty ->
                                ( acc, restOfTheStack )

                            Node a left right ->
                                if left == empty then
                                    ( a :: acc, Stack.push right restOfTheStack )

                                else
                                    ( acc
                                    , restOfTheStack
                                        |> Stack.push right
                                        |> Stack.push (Node a empty empty)
                                        |> Stack.push left
                                    )
                in
                go newStack acc_
    in
    case tree of
        Empty ->
            []

        Node _ _ _ ->
            go (Stack.push tree Stack.Empty) [] |> List.reverse


dfsPreOrder : BST comparable -> List comparable
dfsPreOrder tree =
    let
        go : Stack (BST comparable) -> List comparable -> List comparable
        go stack acc =
            if stack == Stack.empty then
                acc

            else
                let
                    ( maybeNode, restOfTheStack ) =
                        Stack.pop stack

                    node =
                        Maybe.withDefault Empty maybeNode

                    ( acc_, newStack ) =
                        case node of
                            Empty ->
                                ( acc, restOfTheStack )

                            Node a left right ->
                                ( a :: acc
                                , restOfTheStack
                                    |> Stack.push right
                                    |> Stack.push left
                                )
                in
                go newStack acc_
    in
    case tree of
        Empty ->
            []

        Node _ _ _ ->
            go (Stack.push tree Stack.Empty) [] |> List.reverse


dfsPostOrder : BST comparable -> List comparable
dfsPostOrder tree =
    let
        go : Stack (BST comparable) -> List comparable -> List comparable
        go stack acc =
            if stack == Stack.empty then
                acc

            else
                let
                    ( maybeNode, restOfTheStack ) =
                        Stack.pop stack

                    node =
                        Maybe.withDefault Empty maybeNode

                    ( acc_, newStack ) =
                        case node of
                            Empty ->
                                ( acc, restOfTheStack )

                            Node a left right ->
                                if left == empty && right == empty then
                                    ( a :: acc, restOfTheStack )

                                else
                                    ( acc
                                    , restOfTheStack
                                        |> Stack.push (Node a empty empty)
                                        |> Stack.push right
                                        |> Stack.push left
                                    )
                in
                go newStack acc_
    in
    case tree of
        Empty ->
            []

        Node _ _ _ ->
            go (Stack.push tree Stack.Empty) [] |> List.reverse


map : (comparable -> comparable) -> BST comparable -> BST comparable
map f tree =
    case tree of
        Empty ->
            Empty

        Node a left right ->
            Node (f a) (map f left) (map f right)
