module Dec2 exposing (init)


type RockPaperScissors
    = Rock
    | Paper
    | Scissors


type PlayerOutcome
    = Win
    | Loss
    | Draw


type alias RoundSummary =
    ( RockPaperScissors, RockPaperScissors )


input : String
input =
    """
A Y
B X
C Z
"""


toPlayerOutcome : RoundSummary -> PlayerOutcome
toPlayerOutcome ( a, b ) =
    case ( a, b ) of
        ( Scissors, Rock ) ->
            Win

        ( Rock, Paper ) ->
            Win

        ( Paper, Scissors ) ->
            Win

        _ ->
            if a == b then
                Draw

            else
                Loss


rpsToInt : RockPaperScissors -> Int
rpsToInt rps =
    case rps of
        Rock ->
            1

        Paper ->
            2

        Scissors ->
            3


playerOutcomeToInt : PlayerOutcome -> Int
playerOutcomeToInt po =
    case po of
        Win ->
            6

        Draw ->
            3

        Loss ->
            0


stringToRPS : String -> Maybe RockPaperScissors
stringToRPS str =
    case str of
        "A" ->
            Just Rock

        "B" ->
            Just Paper

        "C" ->
            Just Scissors

        "X" ->
            Just Rock

        "Y" ->
            Just Paper

        "Z" ->
            Just Scissors

        _ ->
            Nothing


toGroupedStrings : String -> List ( String, String ) -> List ( String, String )
toGroupedStrings str acc =
    case String.split " " str of
        [ opponent, player ] ->
            ( opponent, player ) :: acc

        _ ->
            acc


combineMaybes : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combineMaybes ( a, b ) =
    Maybe.map2 Tuple.pair a b


toRoundSummary : ( String, String ) -> Maybe RoundSummary
toRoundSummary =
    Tuple.mapBoth stringToRPS stringToRPS >> combineMaybes


toPlayerRoundScore : RoundSummary -> Int
toPlayerRoundScore (( _, playerRPS ) as summary) =
    (toPlayerOutcome summary |> playerOutcomeToInt) + rpsToInt playerRPS


parse : String -> List RoundSummary
parse str =
    String.trim str
        |> String.lines
        |> List.foldl toGroupedStrings []
        |> List.filterMap toRoundSummary


init : Int
init =
    parse input
        |> List.map toPlayerRoundScore
        |> List.sum
