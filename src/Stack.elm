module Stack exposing (..)


type Stack a
    = Node a (Stack a)
    | Empty


pop : Stack a -> ( Maybe a, Stack a )
pop stack =
    case stack of
        Empty ->
            ( Nothing, Empty )

        Node x xs ->
            ( Just x, xs )


push : a -> Stack a -> Stack a
push a stack =
    case stack of
        Empty ->
            Node a Empty

        Node x xs ->
            Node a (push x xs)


map : (a -> b) -> Stack a -> Stack b
map f stack =
    case stack of
        Empty ->
            Empty

        Node a xs ->
            Node (f a) (map f xs)


filter : (a -> Bool) -> Stack a -> Stack a
filter pred stack =
    case stack of
        Empty ->
            Empty

        Node x xs ->
            if pred x then
                Node x (filter pred xs)

            else
                filter pred xs


size : Stack a -> Int
size list =
    case list of
        Empty ->
            0

        Node _ xs ->
            1 + size xs


empty =
    Empty
