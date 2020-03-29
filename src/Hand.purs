module Hand (
    Suit(..),
    showSuit,
    Level,
    toLevel,
    Tricks,
    toTricks,
    defaultTricks,
    fromTricks,
    HonoursType(..),
    Honours(..),
    Doubled(..),
    Hand,
    newHand,
    setDeclarer,
    setAllPass,
    setSuit,
    setLevel,
    setTricks,
    setHonours,
    validHonours,
    setDoubled,
    renderHand,
    renderHandResult,
    scoreHand
) where

import Prelude
    (class Eq, class Ord, const, between, otherwise, show, pure, not, join,
    ($), (&&), (+), (*), (-), (>=), (==), (/=), (<>), (<$>), (<@>), (<*>), (<=))
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Data.Either (Either(..))
import Data.List (List(..))
import Data.String.CodeUnits (singleton) as String
import Data.String.Utils (padStart, padEnd) as String
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup (toEither) as V
import Score (ScoreEntry, Team, opposition, getTeam, Vulnerability)

-- 5 Possible Suit Bids
data Suit = Clubs | Diamonds | Hearts | Spades | NoTrumps

derive instance eqSuit :: Eq Suit

-- Render a suit
showSuit :: Suit -> String
showSuit Clubs = "♣"
showSuit Diamonds = "♦"
showSuit Hearts = "♥"
showSuit Spades = "♠"
showSuit NoTrumps = "🚫"

-- Classification of suits by point value
data SuitType = Minor | Major | NT

-- Get classification
suitType :: Suit -> SuitType
suitType Clubs = Minor
suitType Diamonds = Minor
suitType Hearts = Major
suitType Spades = Major
suitType NoTrumps = NT

-- Difference between first level and subsequent levels
bonusValue :: SuitType -> Int
bonusValue NT = 10
bonusValue _ = 0

-- Value of each level
suitValue :: SuitType -> Int
suitValue Minor = 20
suitValue _ = 30

-- Bidding level
newtype Level = Level Int

derive instance eqLevel :: Eq Level

toLevel :: Int -> Maybe Level
toLevel x   | between 1 7 x     = Just $ Level x
            | otherwise         = Nothing

-- Tricks won
newtype Tricks = Tricks Int

toTricks :: Int -> Maybe Tricks
toTricks x  | between 0 13 x    = Just $ Tricks x
            | otherwise         = Nothing

defaultTricks :: Level -> Tricks
defaultTricks (Level x) = Tricks (x+6)

fromTricks :: Tricks -> Int
fromTricks (Tricks x) = x

-- Result of a hand
data Result = WonBy Int | LostBy Int

-- Get result of contract
result :: Level -> Maybe Tricks -> Result
result _ Nothing = WonBy 0 -- Default value if tricks not specified
result (Level x) (Just (Tricks y))
    | y >= x + 6    = WonBy $ y - x - 6
    | otherwise     = LostBy $ x + 6 - y

-- Render result of contract
renderResult :: Result -> String
renderResult (WonBy amount) = "+" <> show amount
renderResult (LostBy amount) = "-" <> show amount

-- Honours held
data HonoursType = Aces | Four | Five

derive instance eqHonoursType :: Eq HonoursType
derive instance ordHonoursType :: Ord HonoursType

-- Check validity of honours selection
validHonours :: HonoursType -> Suit -> Boolean
validHonours Aces NoTrumps = true
validHonours _ NoTrumps = false
validHonours Aces _ = false
validHonours _ _ = true

-- Points won by holder
honoursValue :: HonoursType -> Int
honoursValue Four = 100
honoursValue _ = 150

data Honours = None | Hons Team HonoursType

derive instance eqHonours :: Eq Honours
derive instance ordHonours :: Ord Honours

validHons :: Honours -> Suit -> Boolean
validHons (Hons _ honours) = validHonours honours
validHons _ = const true

-- Doubling
data Doubled = Undoubled | Doubled | Redoubled

doubleFactor :: Doubled -> Int
doubleFactor Undoubled = 1
doubleFactor Doubled = 2
doubleFactor Redoubled = 4

derive instance eqDoubled :: Eq Doubled
derive instance ordDoubled :: Ord Doubled

-- All data for one hand, possibly incomplete
type Hand = {
    allPass :: Boolean,
    declarer :: Maybe Char,
    team :: Maybe Team,
    suit :: Maybe Suit,
    level :: Maybe Level,
    tricks :: Maybe Tricks,
    honours :: Maybe Honours,
    doubled :: Doubled
}

-- Default hand
newHand :: Hand
newHand = {
    allPass : false,
    declarer : Nothing,
    team : Nothing,
    suit : Nothing,
    level : Nothing,
    tricks : Nothing,
    honours : Nothing,
    doubled : Undoubled
}

-- Set the declarer of a hand
setDeclarer :: Char -> Team -> Hand -> Hand
setDeclarer declarer team = _ {
    allPass = false,
    declarer = Just declarer,
    team = Just team
}

-- Everyone passed
setAllPass :: Hand -> Hand
setAllPass = _ {
    allPass = true,
    declarer = Nothing,
    team = Nothing
}

-- Set the suit bid
setSuit :: Suit -> Hand -> Hand
setSuit suit hand = case hand.honours of
    Just (Hons _ hons) | not $ validHonours hons suit ->
        hand { suit = Just suit, honours = Nothing }
    _ -> hand { suit = Just suit }

-- Set the level bid
setLevel :: Level -> Hand -> Hand
setLevel level = _ { level = Just level }

-- Set the number of tricks bid
setTricks :: Tricks -> Hand -> Hand
setTricks tricks = _ { tricks = Just tricks }

-- Set the honours bonus won
setHonours :: Honours -> Hand -> Hand
setHonours honours@(Hons _ hons) hand = case hand.suit of
    Just suit | not $ validHonours hons suit -> hand { honours = Nothing }
    _ -> hand { honours = Just honours }
setHonours honours hand = hand { honours = Just honours }

-- Set whether the contract was doubled
setDoubled :: Doubled -> Hand -> Hand
setDoubled doubled = _ { doubled = doubled }

-- Render the name of a contract
renderContract :: forall t. { declarer :: Maybe Char, level :: Maybe Level,
    suit :: Maybe Suit, doubled :: Doubled | t } -> String
renderContract contract = declarer <> level <> suit <> doubling where
    declarer = Maybe.maybe " " String.singleton contract.declarer
    level = case contract.level of
        Nothing -> " "
        Just (Level lev) -> show lev
    suit = Maybe.maybe " " showSuit contract.suit
    doubling = case contract.doubled of
        Undoubled -> ""
        Doubled -> "X"
        Redoubled -> "XX"

-- Render the result of a hand
renderHand :: Hand -> String
renderHand hand | hand.allPass = "AP"
                | otherwise =
    contract <> results <> honours where
        contract = String.padEnd 5 $ renderContract hand
        results = String.padStart 3 $ Maybe.maybe "" renderResult $
            result <$> hand.level <@> hand.tricks
        honours = case hand.honours of
            Just (Hons _ _) -> "h"
            _ -> " "

-- Render the result of a hand with the number of tricks won
renderHandResult :: Hand -> String
renderHandResult hand
    | hand.allPass = "AP"
    | otherwise = renderHand hand <> case hand.tricks of
        Just (Tricks x) -> " (" <> show x <> ")"
        Nothing -> case hand.level of
            Just (Level x) -> " (" <> show (x+6) <> ")"
            Nothing -> ""

type Errors = Array String

-- Assign scores to a hand given all details of the hand
scoreHand' :: Char -> Team -> Suit -> Level -> Maybe Tricks -> Honours ->
    Doubled -> Either Errors (Vulnerability -> List (ScoreEntry String))
scoreHand' declarer team suit level tricks hons doubled
    | not $ validHons hons suit =
        Left ["Invalid choice of Honours for this Suit"]
    | otherwise = Right $ \vuln -> let
        -- Name of contract, used for tags
        contract = renderContract {
            declarer : Just declarer,
            level : Just level,
            suit : Just suit,
            doubled
        }
        vulnerable = getTeam team vuln
        Level levelnum = level
        below = pure {
            team,
            below : true,
            {-  Points below are simply suit value * amount * doubling, except
                notrumps adds 10 points -}
            amount : doubleFactor doubled *
                (levelnum * suitValue (suitType suit) +
                    bonusValue (suitType suit)),
            source : "Won " <> contract
        }
        overBonus x
            | x == 0 = Nil -- No overtricks won, no points
            | doubled == Undoubled = pure {
                    team,
                    below : false,
                    -- Undoubled overtricks are simply suit value * amount
                    amount : x * suitValue (suitType suit),
                    source : contract <> " with " <> show x <> " overtrick(s)"
                }
            | vulnerable = pure {
                    team,
                    below : false,
                    {-  Vulnerable overtricks are 200 each doubled,
                        400 each redoubled -}
                    amount : 100 * doubleFactor doubled * x,
                    source : contract <> " with " <> show x <>
                        " overtrick(s) and vulnerable"
                }
            | otherwise = pure {
                    team,
                    below : false,
                    {-  Not vulnerable overtricks are 100 each doubled,
                        200 each redoubled -}
                    amount : 50 * doubleFactor doubled * x,
                    source : contract <> " with " <> show x <> " overtrick(s)"
                }
        -- Bonus of 50 points for winning a doubled contract, 100 for redoubled
        insult
            | doubled == Undoubled = Nil
            | otherwise = pure {
                    team,
                    below : false,
                    amount : doubleFactor doubled * 25,
                    source : contract <> " insult bonus"
                }
        slam
            | levelnum >= 6 && vulnerable = pure {
                    team,
                    below : false,
                    -- Vulnerable slams are 750 for baby and 1500 for grand
                    amount : 750 * (levelnum - 5),
                    source : contract <> " slam bonus and vulnerable"
                }
            | levelnum >= 6 = pure {
                    team,
                    below : false,
                    -- Not vulnerable slams are 500 for baby and 1000 for grand
                    amount : 500 * (levelnum - 5),
                    source : contract <> " slam bonus"
                }
            | otherwise = Nil -- Didn't bid 6 or 7, no slam
        honours = case hons of
            None -> Nil
            Hons honsteam Four -> pure {
                    team : honsteam,
                    below : false,
                    amount : honoursValue Four,
                    source : contract <> " honours (4 out of 5)"
                }
            Hons honsteam Five -> pure {
                    team : honsteam,
                    below : false,
                    amount : honoursValue Five,
                    source : contract <> " honours (5 out of 5)"
                }
            Hons honsteam Aces -> pure {
                    team : honsteam,
                    below : false,
                    amount : honoursValue Aces,
                    source : contract <> " honours (aces)"
                }
        -- calculate points for undertricks with tail call recursion
        underPoints x tot
            | x <= 0 = tot -- base case, return running total
            | doubled /= Undoubled && x >= 4 = underPoints (x-1) $
                -- 4th and later are worth 300 doubled, 600 redoubled
                tot + 150 * doubleFactor doubled
            | doubled /= Undoubled && x >= 2 && vulnerable = underPoints (x-1) $
                {-  2nd and 3rd are worth 300 doubled, 600 redoubled,
                    when declarer is vulnerable -}
                tot + 150 * doubleFactor doubled
            | doubled /= Undoubled && x >= 2 = underPoints (x-1) $
                {-  2nd and 3rd are worth 200 doubled, 400 redoubled,
                    when declarer is not vulnerable -}
                tot + 100 * doubleFactor doubled
            {-  1st is worth 200 doubled, 400 redoubled,
                and all undoubled are worth 100, when declarer is vulnerable -}
            | vulnerable = tot + 100 * doubleFactor doubled * x
            {-  1st is worth 100 doubled, 200 redoubled, and
                all undoubled are worth 50, when declarer is not vulnerable -}
            | otherwise = tot + 50 * doubleFactor doubled * x
        unders x = pure {
            team : opposition team,
            below : false,
            amount : underPoints x 0,
            source : "Lost " <> contract <> " with " <> show x <> " undertrick(s)"
        }
    in case result level tricks of
        {-  If contract won, give points for the contract, overtricks,
            insult bonus, slam bonus, and honours -}
        WonBy over -> below <> overBonus over <> insult <> slam <> honours
        -- If contract lost, give points for the undertricks and honours
        LostBy under -> unders under <> honours

-- Give an error message for an absent value
withError :: forall a b. Maybe a -> b -> V (Array b) a
withError Nothing e = V $ Left [e]
withError (Just x) _ = V $ Right x

-- Get a score generator for a hand, or errors if the hand cannot be scored
scoreHand :: Hand -> Either Errors (Vulnerability -> List (ScoreEntry String))
scoreHand hand
    | hand.allPass = pure $ const Nil
    | otherwise = join $ V.toEither scores
    where
        scores = scoreHand'
            <$> (hand.declarer `withError` "Declarer was not chosen")
            <*> (hand.team `withError` "Declarer's team was not set")
            <*> (hand.suit `withError` "Bid suit was not chosen")
            <*> (hand.level `withError` "Bid level was not chosen")
            -- Default is for contract to be won with no overtricks
            <@> hand.tricks
            <*> (hand.honours `withError` "Honours has not been selected")
            <@> hand.doubled -- Default is undoubled
