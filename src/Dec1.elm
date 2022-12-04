module Dec1 exposing (init)


input : String
input =
    """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""


toGroupedLists : String -> List (List Int) -> List (List Int)
toGroupedLists n acc =
    case ( String.toInt n, acc ) of
        ( Nothing, xs ) ->
            [] :: xs

        ( Just x, [] ) ->
            [ List.singleton x ]

        ( Just x, head :: tail ) ->
            (x :: head) :: tail


parse : String -> List (List Int)
parse str =
    String.trim str
        |> String.lines
        |> List.foldr toGroupedLists []


desc : Int -> Int -> Order
desc a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


toTopThreeSum : List (List Int) -> Int
toTopThreeSum xs =
    List.map List.sum xs
        |> List.sortWith desc
        |> List.take 3
        |> List.sum


init : Int
init =
    parse input
        |> toTopThreeSum
