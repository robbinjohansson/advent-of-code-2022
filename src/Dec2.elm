module Dec2 exposing
    ( initWithFormula
    , initWithoutFormula
    )


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


toRoundSummaryWithFormula : ( String, String ) -> Maybe RoundSummary
toRoundSummaryWithFormula ( opponent, player ) =
    case ( opponent, player ) of
        ( "A", "X" ) ->
            Just ( Rock, Scissors )

        ( "A", "Y" ) ->
            Just ( Rock, Rock )

        ( "A", "Z" ) ->
            Just ( Rock, Paper )

        ( "B", "X" ) ->
            Just ( Paper, Rock )

        ( "B", "Y" ) ->
            Just ( Paper, Paper )

        ( "B", "Z" ) ->
            Just ( Paper, Scissors )

        ( "C", "X" ) ->
            Just ( Scissors, Paper )

        ( "C", "Y" ) ->
            Just ( Scissors, Scissors )

        ( "C", "Z" ) ->
            Just ( Scissors, Rock )

        _ ->
            Nothing


toPlayerRoundScore : RoundSummary -> Int
toPlayerRoundScore summary =
    let
        playerOutcomeScore =
            toPlayerOutcome summary
                |> playerOutcomeToInt

        ( _, rpsScore ) =
            Tuple.mapSecond rpsToInt summary
    in
    playerOutcomeScore + rpsScore


parse : String -> (( String, String ) -> Maybe RoundSummary) -> List RoundSummary
parse str f =
    String.trim str
        |> String.lines
        |> List.foldl toGroupedStrings []
        |> List.filterMap f


init : (( String, String ) -> Maybe RoundSummary) -> Int
init f =
    parse input f
        |> List.map toPlayerRoundScore
        |> List.sum


initWithFormula : Int
initWithFormula =
    init toRoundSummaryWithFormula


initWithoutFormula : Int
initWithoutFormula =
    init toRoundSummary
