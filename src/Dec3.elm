module Dec3 exposing
    ( part1
    , part2
    )

import Set exposing (Set)


input : String
input =
    """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"""


toListOfSets : List Char -> List ( Set Char, Set Char ) -> List ( Set Char, Set Char )
toListOfSets xs acc =
    let
        delimiter =
            List.length xs // 2
    in
    ( List.take delimiter xs |> Set.fromList
    , List.drop delimiter xs |> Set.fromList
    )
        :: acc


parse : String -> List (List Char)
parse str =
    String.trim str
        |> String.lines
        |> List.map String.toList


toListOfCommonChars : ( Set Char, Set Char ) -> List Char
toListOfCommonChars =
    uncurry Set.intersect >> Set.toList


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


toPriority : Char -> Int
toPriority str =
    case str of
        'a' ->
            1

        'b' ->
            2

        'c' ->
            3

        'd' ->
            4

        'e' ->
            5

        'f' ->
            6

        'g' ->
            7

        'h' ->
            8

        'i' ->
            9

        'j' ->
            10

        'k' ->
            11

        'l' ->
            12

        'm' ->
            13

        'n' ->
            14

        'o' ->
            15

        'p' ->
            16

        'q' ->
            17

        'r' ->
            18

        's' ->
            19

        't' ->
            20

        'u' ->
            21

        'v' ->
            22

        'w' ->
            23

        'x' ->
            24

        'y' ->
            25

        'z' ->
            26

        'A' ->
            27

        'B' ->
            28

        'C' ->
            29

        'D' ->
            30

        'E' ->
            31

        'F' ->
            32

        'G' ->
            33

        'H' ->
            34

        'I' ->
            35

        'J' ->
            36

        'K' ->
            37

        'L' ->
            38

        'M' ->
            39

        'N' ->
            40

        'O' ->
            41

        'P' ->
            42

        'Q' ->
            43

        'R' ->
            44

        'S' ->
            45

        'T' ->
            46

        'U' ->
            47

        'V' ->
            48

        'W' ->
            49

        'X' ->
            50

        'Y' ->
            51

        'Z' ->
            52

        _ ->
            0


toGroups : List a -> List (List a)
toGroups xs =
    case xs of
        [] ->
            []

        _ ->
            List.take 3 xs :: toGroups (List.drop 3 xs)


toMaybeCommonChar : List (List Char) -> Maybe Char
toMaybeCommonChar xs =
    case List.map Set.fromList xs of
        [ x1, x2, x3 ] ->
            Set.intersect x1 x2
                |> Set.intersect x3
                |> Set.toList
                |> List.head

        _ ->
            Nothing


part1 : Int
part1 =
    parse input
        |> List.foldl toListOfSets []
        |> List.concatMap toListOfCommonChars
        |> List.map toPriority
        |> List.sum


part2 : Int
part2 =
    parse input
        |> toGroups
        |> List.filterMap toMaybeCommonChar
        |> List.map toPriority
        |> List.sum
