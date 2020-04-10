module Score (
    Team(..),
    opposition,
    TeamValue,
    startTeams,
    getTeam,
    adjustTeam,
    Vulnerability,
    ScoreEntry,
    ScoreTotal,
    startScore,
    scoreAll
) where

import Prelude
    (class Eq, class Ord, class Show, not, otherwise,
    (&&), (>=), (+), ($), (<>))
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldl)

data Team = We | They

derive instance eqTeam :: Eq Team
derive instance ordTeam :: Ord Team
instance showTeam :: Show Team where
    show We = "We"
    show They = "They"

opposition :: Team -> Team
opposition We = They
opposition They = We

-- A value for both We and They
type TeamValue t = { we :: t, they :: t }

-- Set a value for both teams to start on
startTeams :: forall t. t -> TeamValue t
startTeams x = { we : x, they : x }

-- Get the value for one team
getTeam :: forall t. Team -> TeamValue t -> t
getTeam We = _.we
getTeam They = _.they

-- Adjust the value for one team
adjustTeam :: forall t. (t -> t) -> Team -> TeamValue t -> TeamValue t
adjustTeam f We teamvalue = teamvalue { we = f teamvalue.we }
adjustTeam f They teamvalue = teamvalue { they = f teamvalue.they }

type Vulnerability = TeamValue Boolean

-- A tagged entry on the scorepad
type ScoreEntry s = {
    team :: Team, -- Which team the score is given to
    below :: Boolean, -- Is the score placed below the line
    amount :: Int, -- How much is the score worth
    source :: s -- Tag
}

-- Current state of the scoreboard
type ScoreTotal = {
    vulnerable :: Vulnerability, -- Who is vulnerable
    below :: TeamValue Int, -- Total below the lowest line
    total :: TeamValue Int -- Total on the whole scorepad
}

-- Initial state of scoreboard
startScore :: ScoreTotal
startScore = {
    vulnerable : startTeams false,
    below : startTeams 0,
    total : startTeams 0
}

-- Check if adding a score will give that team a game
doesGame :: forall s. ScoreTotal -> ScoreEntry s -> Boolean
doesGame total entry =
    entry.below &&
    getTeam entry.team total.below + entry.amount >= 100

-- Get the rubber bonus entry if it needs to be added
rubberBonus :: forall s. s -> ScoreTotal -> ScoreEntry s -> Maybe (ScoreEntry s)
rubberBonus rbsource total entry = bonus where
    bonus
        | doesGame total entry && decVuln =
            Just {
                team : entry.team,
                below : false,
                amount,
                source : rbsource
            }
        | otherwise = Nothing
    newBelow = getTeam entry.team total.below + entry.amount
    decVuln = getTeam entry.team total.vulnerable
    oppVuln = getTeam (opposition entry.team) total.vulnerable
    amount
        | oppVuln   = 500
        | otherwise = 700

-- The state of the function to iteratively get all scores
type TotalState s = {
    -- List of entries to be put on the scorepad, with Nothing indicating a completed game
    entries :: Array (Maybe (ScoreEntry s)),
    -- Current totals
    totals :: ScoreTotal,
    -- Tag to be added to rubber bonuses
    rbsource :: s
}

-- Start state of the function to iteratively get all scores
startState :: forall s. s -> TotalState s
startState rbsource = {
    entries : [],
    totals : startScore,
    rbsource
}

-- Add a single score to the state
scoreEntry :: forall s. TotalState s -> ScoreEntry s -> TotalState s
scoreEntry state entry = case rubberBonus state.rbsource state.totals entry of
    Just rubber -> -- Rubber won
        scoreEntry { -- add the rubber bonus to the state next
            entries : state.entries <> [Just entry, Nothing], -- Add this score and a line below
            totals : {
                vulnerable : startTeams false, -- New rubber, no one vulnerable
                below : startTeams 0, -- New line, no scores below
                total : adjustTeam (_ + entry.amount) entry.team state.totals.total -- Add score
            },
            rbsource : state.rbsource
        } rubber
    _ | doesGame state.totals entry -> { -- Game won
        entries : state.entries <> [Just entry, Nothing], -- Add this score and a line below
        totals : {
            vulnerable : adjustTeam not entry.team state.totals.vulnerable, -- Flip vulnerability of winning team
            below : startTeams 0, -- New line, no scores below
            total : adjustTeam (_ + entry.amount) entry.team state.totals.total -- Add score
        },
        rbsource : state.rbsource
    }
    _ | entry.below -> { -- Points below the line
        entries : state.entries <> [Just entry], -- Add this score
        totals : { -- Only update scores
            vulnerable : state.totals.vulnerable,
            below : adjustTeam (_ + entry.amount) entry.team state.totals.below,
            total : adjustTeam (_ + entry.amount) entry.team state.totals.total
        },
        rbsource : state.rbsource
    }
    _ -> { -- Points above the line
        entries : state.entries <> [Just entry], -- Add this score
        totals : state.totals { -- Only uodate total points
            total = adjustTeam (_ + entry.amount) entry.team state.totals.total
        },
        rbsource : state.rbsource
    }

-- Add scores from a single generating function to the state
scoreStep :: forall f s. Foldable f =>
    TotalState s -> (Vulnerability -> f (ScoreEntry s)) -> TotalState s
scoreStep state gen =
    foldl scoreEntry state $ gen state.totals.vulnerable

-- Add scores from a list of generating functions to the scorepad
scoreAll :: forall f s. Foldable f =>
    s -> f (Vulnerability -> f (ScoreEntry s)) ->
    {entries :: Array (Maybe (ScoreEntry s)), totals :: ScoreTotal}
scoreAll rbsource list = {
        entries : foldres.entries,
        totals : foldres.totals
    } where
        foldres = foldl scoreStep (startState rbsource) list
