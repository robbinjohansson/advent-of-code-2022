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


foldMax : List Int -> Int -> Int
foldMax xs acc =
    max (List.sum xs) acc


findMaximum : List (List Int) -> Int
findMaximum =
    List.foldl foldMax 0


init : Int
init =
    parse input
        |> findMaximum
