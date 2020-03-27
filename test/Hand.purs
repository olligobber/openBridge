module Test.Hand where

import Prelude
    (($), (<$>), (<@>), (#), (==),
    Unit, class Eq, discard, map, bind, pure, negate)
import Data.Maybe (Maybe(..), isNothing, isJust, fromJust)
import Data.Either (Either(..), isRight, fromRight)
import Data.List (List(..), (:))
import Effect (Effect)
import Data.Foldable (any)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as A
import Partial.Unsafe (unsafePartial)
import Data.Traversable (traverse)

import Hand
    (Doubled(..), Honours(..), HonoursType(..), Suit(..),
    newHand, renderHand, scoreHand, toLevel, toTricks,
    setDeclarer, setDoubled, setHonours, setLevel, setSuit, setTricks)
import Score (Team(..), scoreAll, startTeams)

leftEq :: forall a b. (Eq a) => a -> Either a b -> Boolean
leftEq x (Left y) = x == y
leftEq _ (Right _) = false

main :: Effect Unit
main = runTest do
    suite "Validating hands" do
        test "Missing declarer" $
            A.assert "Expect error" $ any (leftEq
                ["Declarer was not chosen", "Declarer's team was not set"])
                do
                    level <- toLevel 3
                    newHand
                        # setLevel level
                        # setSuit Hearts
                        # setHonours (Hons We Five)
                        # scoreHand
                        # pure
        test "Missing suit" $
            A.assert "Expect error" $
                any (leftEq ["Bid suit was not chosen"]) do
                    level <- toLevel 4
                    tricks <- toTricks 11
                    newHand
                        # setDeclarer 'W' They
                        # setLevel level
                        # setTricks tricks
                        # setHonours None
                        # scoreHand
                        # pure
        test "Missing level" $
            A.assert "Expect error" $
                leftEq ["Bid level was not chosen"] $ scoreHand $
                    setDeclarer 'N' We $
                    setSuit NoTrumps $
                    setHonours (Hons They Aces)
                    newHand
        test "Missing honours" $
            A.assert "Expect error" $
                any (leftEq ["Honours has not been selected"]) do
                    level <- toLevel 1
                    tricks <- toTricks 8
                    newHand
                        # setDeclarer 'S' They
                        # setLevel level
                        # setTricks tricks
                        # setSuit NoTrumps
                        # scoreHand
                        # pure
        test "Multiple fields missing" $
            A.assert "Multiple errors combine" $
                any (leftEq
                    [ "Declarer was not chosen"
                    , "Declarer's team was not set"
                    , "Bid suit was not chosen"
                    , "Bid level was not chosen"
                    , "Honours has not been selected"
                    ]
                ) do
                    tricks <- toTricks 1
                    newHand
                        # setTricks tricks
                        # setDoubled Redoubled
                        # scoreHand
                        # pure
        test "Bid at level 8" $
            A.assert "Expect error" $ isNothing do
                level <- toLevel 8
                newHand
                    # setDeclarer 'N' They
                    # setLevel level
                    # setSuit Hearts
                    # setHonours (Hons They Five)
                    # scoreHand
                    # pure
        test "Bid at level 0" $
            A.assert "Expect error" $ isNothing do
                level <- toLevel 0
                tricks <- toTricks 7
                newHand
                    # setDeclarer 'E' We
                    # setLevel level
                    # setTricks tricks
                    # setSuit NoTrumps
                    # setHonours None
                    # setDoubled Doubled
                    # scoreHand
                    # pure
        test "Won 14 tricks" $
            A.assert "Expect error" $ isNothing do
                level <- toLevel 7
                tricks <- toTricks 14
                newHand
                    # setDeclarer 'N' We
                    # setLevel level
                    # setTricks tricks
                    # setSuit Spades
                    # setHonours (Hons We Five)
                    # setDoubled Redoubled
                    # scoreHand
                    # pure
        test "Won negative tricks" $
            A.assert "Expect error" $ isNothing do
                level <- toLevel 1
                tricks <- toTricks (-1)
                newHand
                    # setDeclarer 'W' They
                    # setLevel level
                    # setTricks tricks
                    # setSuit NoTrumps
                    # setHonours (Hons We Aces)
                    # scoreHand
                    # pure
        test "Ace honours set after choosing suit" $
            A.assert "Expect error" $
                any (leftEq ["Honours has not been selected"]) do
                    level <- toLevel 3
                    newHand
                        # setDeclarer 'E' We
                        # setLevel level
                        # setSuit Spades
                        # setHonours (Hons We Aces)
                        # scoreHand
                        # pure
        test "Four honours set after choosing no trumps" $
            A.assert "Expect error" $
                any (leftEq ["Honours has not been selected"]) do
                    level <- toLevel 4
                    newHand
                        # setDeclarer 'W' They
                        # setLevel level
                        # setSuit NoTrumps
                        # setHonours (Hons We Four)
                        # scoreHand
                        # pure
        test "No trumps chosen after setting five honours" $
            A.assert "Expect error" $
                any (leftEq ["Honours has not been selected"]) do
                    level <- toLevel 1
                    tricks <- toTricks 8
                    newHand
                        # setHonours (Hons We Five)
                        # setDeclarer 'N' They
                        # setSuit NoTrumps
                        # setLevel level
                        # setTricks tricks
                        # scoreHand
                        # pure
        test "Suit chosen after setting ace honours" $
            A.assert "Ace honours deleted when selecting suit" $
                any (leftEq ["Honours has not been selected"]) do
                    level <- toLevel 3
                    tricks <- toTricks 7
                    newHand
                        # setSuit NoTrumps
                        # setHonours (Hons They Aces)
                        # setDeclarer 'S' We
                        # setSuit Diamonds
                        # setLevel level
                        # setTricks tricks
                        # scoreHand
                        # pure
        test "Validates N3🚫X+0" $
            A.assert "Hand created successfully" $ any isRight do
                level <- toLevel 3
                tricks <- toTricks 9
                newHand
                    # setDeclarer 'N' They
                    # setLevel level
                    # setSuit NoTrumps
                    # setTricks tricks
                    # setHonours None
                    # setDoubled Doubled
                    # scoreHand
                    # pure
        test "Validates S7♥+0h" $
            A.assert "Hand created successfully" $ any isRight do
                level <- toLevel 7
                newHand
                    # setDeclarer 'S' We
                    # setLevel level
                    # setSuit Hearts
                    # setHonours (Hons We Four)
                    # scoreHand
                    # pure
        test "Validates E1♣XX-4h" $
            A.assert "Hand created successfully" $ any isRight do
                level <- toLevel 1
                tricks <- toTricks 2
                newHand
                    # setDeclarer 'E' They
                    # setLevel level
                    # setSuit Clubs
                    # setTricks tricks
                    # setHonours (Hons They Five)
                    # setDoubled Redoubled
                    # scoreHand
                    # pure
    suite "Rendering incomplete hands" do
        test "Only selected declarer" $
            A.equal "N        " $ renderHand $ setDeclarer 'N' We newHand
        test "Only selected level" $
            A.equal (Just " 4    +0 ") $
                map renderHand $ setLevel <$> toLevel 4 <@> newHand
        test "Only selected suit" $
            A.equal "  ♠      " $ renderHand $ setSuit Spades newHand
        test "Only selected double" $
            A.equal "   X     " $ renderHand $ setDoubled Doubled newHand
        test "Only selected redouble" $
            A.equal "   XX    " $ renderHand $ setDoubled Redoubled newHand
        test "Selected max level min tricks" $
            A.equal (Just " 7   -13 ") do
                level <- toLevel 7
                tricks <- toTricks 0
                newHand
                    # setLevel level
                    # setTricks tricks
                    # renderHand
                    # pure
        test "Selected min level max tricks" $
            A.equal (Just " 1    +6 ") do
                level <- toLevel 1
                tricks <- toTricks 13
                newHand
                    # setLevel level
                    # setTricks tricks
                    # renderHand
                    # pure
        test "Only selected honours" $
            A.equal "        h" $
                renderHand $ setHonours (Hons We Four) newHand
    suite "Rendering complete hands" do
        test "Rendering N1♠XX+6h" $
            A.equal (Just "N1♠XX +6h") do
                level <- toLevel 1
                tricks <- toTricks 13
                newHand
                    # setDeclarer 'N' We
                    # setLevel level
                    # setSuit Spades
                    # setDoubled Redoubled
                    # setHonours (Hons We Five)
                    # setTricks tricks
                    # renderHand
                    # pure
        test "Rendering S4🚫-10" $
            A.equal (Just "S4🚫  -10 ") do
                level <- toLevel 4
                tricks <- toTricks 0
                newHand
                    # setDeclarer 'S' They
                    # setLevel level
                    # setSuit NoTrumps
                    # setTricks tricks
                    # renderHand
                    # pure
        test "Rendering E2♣+0" $
            A.equal (Just "E2♣   +0 ") do
                level <- toLevel 2
                newHand
                    # setDeclarer 'E' We
                    # setLevel level
                    # setSuit Clubs
                    # setHonours None
                    # renderHand
                    # pure
    suite "Scoring hands" do
        test "Scores E2🚫+0" do
            let maybeHand = do
                    level <- toLevel 2
                    newHand
                        # setDeclarer 'E' We
                        # setLevel level
                        # setSuit NoTrumps
                        # setHonours None
                        # pure
            A.assert "Created hand" $ isJust $ maybeHand
            let hand = unsafePartial $ fromJust $ maybeHand
            A.assert "Scored hand" $ isRight $ scoreHand hand
            let scoredHand = unsafePartial $ fromRight $ scoreHand hand
                scorepad = scoreAll "Rubber Bonus" (scoredHand : Nil)
            A.equal
                ( Just {team : We, below : true, amount : 70,
                    source : "Won E2🚫"}
                : Nil
                )
                scorepad.entries
            A.equal (startTeams false) scorepad.totals.vulnerable
            A.equal {we : 70, they : 0} scorepad.totals.below
            A.equal {we : 70, they : 0} scorepad.totals.total
        test "Scores W1♠+1" do
            let maybeHand = do
                    level <- toLevel 1
                    tricks <- toTricks 8
                    newHand
                        # setDeclarer 'W' They
                        # setTricks tricks
                        # setLevel level
                        # setSuit Spades
                        # setHonours None
                        # pure
            A.assert "Created hand" $ isJust $ maybeHand
            let hand = unsafePartial $ fromJust $ maybeHand
            A.assert "Scored hand" $ isRight $ scoreHand hand
            let scoredHand = unsafePartial $ fromRight $ scoreHand hand
                scorepad = scoreAll "Rubber Bonus" (scoredHand : Nil)
            A.equal
                ( Just {team : They, below : true, amount : 30,
                    source : "Won W1♠"}
                : Just {team : They, below : false, amount : 30,
                    source : "W1♠ with 1 overtrick(s)"}
                : Nil
                )
                scorepad.entries
        test "Scores N5♦+2" do
            let maybeHand = do
                    level <- toLevel 5
                    tricks <- toTricks 13
                    newHand
                        # setDeclarer 'N' We
                        # setTricks tricks
                        # setLevel level
                        # setSuit Diamonds
                        # setHonours None
                        # pure
            A.assert "Created hand" $ isJust $ maybeHand
            let hand = unsafePartial $ fromJust $ maybeHand
            A.assert "Scored hand" $ isRight $ scoreHand hand
            let scoredHand = unsafePartial $ fromRight $ scoreHand hand
                scorepad = scoreAll "Rubber Bonus" (scoredHand : Nil)
            A.equal
                ( Just {team : We, below : true, amount : 100,
                    source : "Won N5♦"}
                : Nothing
                : Just {team : We, below : false, amount : 40,
                    source : "N5♦ with 2 overtrick(s)"}
                : Nil
                )
                scorepad.entries
            A.equal {we : true, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 140, they : 0} scorepad.totals.total
        test "Scores S1🚫XX+2" do
            let maybeHand = do
                    level <- toLevel 1
                    tricks <- toTricks 9
                    newHand
                        # setDeclarer 'S' They
                        # setTricks tricks
                        # setLevel level
                        # setSuit NoTrumps
                        # setHonours None
                        # setDoubled Redoubled
                        # pure
            A.assert "Created hand" $ isJust $ maybeHand
            let hand = unsafePartial $ fromJust $ maybeHand
            A.assert "Scored hand" $ isRight $ scoreHand hand
            let scoredHand = unsafePartial $ fromRight $ scoreHand hand
                scorepad = scoreAll "Rubber Bonus" (scoredHand : Nil)
            A.equal
                ( Just {team : They, below : true, amount : 160,
                    source : "Won S1🚫XX"}
                : Nothing
                : Just {team : They, below : false, amount : 400,
                    source : "S1🚫XX with 2 overtrick(s)"}
                : Just {team : They, below : false, amount : 100,
                    source : "S1🚫XX insult bonus"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : true} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 0, they : 660} scorepad.totals.total
        test "Scores W1♥X+1" do
            let maybeHand = do
                    level <- toLevel 1
                    tricks <- toTricks 8
                    newHand
                        # setDeclarer 'W' They
                        # setTricks tricks
                        # setLevel level
                        # setSuit Hearts
                        # setHonours None
                        # setDoubled Doubled
                        # pure
            A.assert "Created hand" $ isJust $ maybeHand
            let hand = unsafePartial $ fromJust $ maybeHand
            A.assert "Scored hand" $ isRight $ scoreHand hand
            let scoredHand = unsafePartial $ fromRight $ scoreHand hand
                scorepad = scoreAll "Rubber Bonus" (scoredHand : Nil)
            A.equal
                ( Just {team : They, below : true, amount : 60,
                    source : "Won W1♥X"}
                : Just {team : They, below : false, amount : 100,
                    source : "W1♥X with 1 overtrick(s)"}
                : Just {team : They, below : false, amount : 50,
                    source : "W1♥X insult bonus"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 60} scorepad.totals.below
            A.equal {we : 0, they : 210} scorepad.totals.total
        test "Scores N6♣+0, S1🚫+0, N2♥-2, S3♦X+1" do
            let maybeHands = do
                    level1 <- toLevel 1
                    level2 <- toLevel 2
                    level3 <- toLevel 3
                    level6 <- toLevel 6
                    tricks6 <- toTricks 6
                    tricks10 <- toTricks 10
                    let hand1 = newHand
                            # setDeclarer 'N' We
                            # setLevel level6
                            # setSuit Clubs
                            # setHonours None
                        hand2 = newHand
                            # setDeclarer 'S' We
                            # setLevel level1
                            # setSuit NoTrumps
                            # setHonours None
                        hand3 = newHand
                            # setDeclarer 'N' We
                            # setLevel level2
                            # setSuit Hearts
                            # setTricks tricks6
                            # setHonours None
                        hand4 = newHand
                            # setDeclarer 'S' We
                            # setLevel level3
                            # setSuit Diamonds
                            # setDoubled Doubled
                            # setTricks tricks10
                            # setHonours None
                    pure (hand1 : hand2 : hand3 : hand4 : Nil)
            A.assert "Created hands" $ isJust $ maybeHands
            let hands = unsafePartial $ fromJust $ maybeHands
            A.assert "Scored hands" $ isRight $ traverse scoreHand hands
            let scoredHands =
                    unsafePartial $ fromRight $ traverse scoreHand hands
                scorepad = scoreAll "Rubber Bonus" scoredHands
            A.equal
                ( Just {team : We, below : true, amount : 120,
                    source : "Won N6♣"}
                : Nothing
                : Just {team : We, below : false, amount : 500,
                    source : "N6♣ slam bonus"}
                : Just {team : We, below : true, amount : 40,
                    source : "Won S1🚫"}
                : Just {team : They, below : false, amount : 200,
                    source : "Lost N2♥ with 2 undertrick(s)"}
                : Just {team : We, below : true, amount : 120,
                    source : "Won S3♦X"}
                : Nothing
                : Just {team : We, below : false, amount : 700,
                    source : "Rubber Bonus"}
                : Just {team : We, below : false, amount : 200,
                    source : "S3♦X with 1 overtrick(s) and vulnerable"}
                : Just {team : We, below : false, amount : 50,
                    source : "S3♦X insult bonus"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 1730, they : 200} scorepad.totals.total
        test "Scores E4♠+1, S7♠+0, N1🚫X-4, N4♥XX+2" do
            let maybeHands = do
                    level1 <- toLevel 1
                    level4 <- toLevel 4
                    level7 <- toLevel 7
                    tricks3 <- toTricks 3
                    tricks11 <- toTricks 11
                    tricks12 <- toTricks 12
                    let hand1 = newHand
                            # setDeclarer 'E' We
                            # setLevel level4
                            # setSuit Spades
                            # setTricks tricks11
                            # setHonours None
                        hand2 = newHand
                            # setDeclarer 'S' They
                            # setLevel level7
                            # setSuit Spades
                            # setHonours None
                        hand3 = newHand
                            # setDeclarer 'N' They
                            # setLevel level1
                            # setSuit NoTrumps
                            # setDoubled Doubled
                            # setTricks tricks3
                            # setHonours None
                        hand4 = newHand
                            # setDeclarer 'N' They
                            # setLevel level4
                            # setSuit Hearts
                            # setDoubled Redoubled
                            # setTricks tricks12
                            # setHonours None
                    pure (hand1 : hand2 : hand3 : hand4 : Nil)
            A.assert "Created hands" $ isJust $ maybeHands
            let hands = unsafePartial $ fromJust $ maybeHands
            A.assert "Scored hands" $ isRight $ traverse scoreHand hands
            let scoredHands =
                    unsafePartial $ fromRight $ traverse scoreHand hands
                scorepad = scoreAll "Rubber Bonus" scoredHands
            A.equal
                ( Just {team : We, below : true, amount : 120,
                    source : "Won E4♠"}
                : Nothing
                : Just {team : We, below : false, amount : 30,
                    source : "E4♠ with 1 overtrick(s)"}
                : Just {team : They, below : true, amount : 210,
                    source : "Won S7♠"}
                : Nothing
                : Just {team : They, below : false, amount : 1000,
                    source : "S7♠ slam bonus"}
                : Just {team : We, below : false, amount : 1100,
                    source : "Lost N1🚫X with 4 undertrick(s)"}
                : Just {team : They, below : true, amount : 480,
                    source : "Won N4♥XX"}
                : Nothing
                : Just {team : They, below : false, amount : 500,
                    source : "Rubber Bonus"}
                : Just {team : They, below : false, amount : 800,
                    source : "N4♥XX with 2 overtrick(s) and vulnerable"}
                : Just {team : They, below : false, amount : 100,
                    source : "N4♥XX insult bonus"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 1250, they : 3090} scorepad.totals.total
        test "Scores W4♠XX-5h, N3🚫+0h, E5♣X-6h, N7🚫X+0h" do
            let maybeHands = do
                    level3 <- toLevel 3
                    level4 <- toLevel 4
                    level5 <- toLevel 5
                    level7 <- toLevel 7
                    tricks <- toTricks 5
                    let hand1 = newHand
                            # setDeclarer 'W' They
                            # setLevel level4
                            # setSuit Spades
                            # setDoubled Redoubled
                            # setTricks tricks
                            # setHonours (Hons We Four)
                        hand2 = newHand
                            # setDeclarer 'N' We
                            # setLevel level3
                            # setSuit NoTrumps
                            # setHonours (Hons We Aces)
                        hand3 = newHand
                            # setDeclarer 'E' They
                            # setLevel level5
                            # setSuit Clubs
                            # setDoubled Doubled
                            # setTricks tricks
                            # setHonours (Hons We Five)
                        hand4 = newHand
                            # setDeclarer 'N' We
                            # setLevel level7
                            # setSuit NoTrumps
                            # setDoubled Doubled
                            # setHonours (Hons We Aces)
                    pure (hand1 : hand2 : hand3 : hand4 : Nil)
            A.assert "Created hands" $ isJust $ maybeHands
            let hands = unsafePartial $ fromJust $ maybeHands
            A.assert "Scored hands" $ isRight $ traverse scoreHand hands
            let scoredHands =
                    unsafePartial $ fromRight $ traverse scoreHand hands
                scorepad = scoreAll "Rubber Bonus" scoredHands
            A.equal
                ( Just {team : We, below : false, amount : 2200,
                    source : "Lost W4♠XX with 5 undertrick(s)"}
                : Just {team : We, below : false, amount : 100,
                    source : "W4♠XX honours (4 out of 5)"}
                : Just {team : We, below : true, amount : 100,
                    source : "Won N3🚫"}
                : Nothing
                : Just {team : We, below : false, amount : 150,
                    source : "N3🚫 honours (aces)"}
                : Just {team : We, below : false, amount : 1400,
                    source : "Lost E5♣X with 6 undertrick(s)"}
                : Just {team : We, below : false, amount : 150,
                    source : "E5♣X honours (5 out of 5)"}
                : Just {team : We, below : true, amount : 440,
                    source : "Won N7🚫X"}
                : Nothing
                : Just {team : We, below : false, amount : 700,
                    source : "Rubber Bonus"}
                : Just {team : We, below : false, amount : 50,
                    source : "N7🚫X insult bonus"}
                : Just {team : We, below : false, amount : 1500,
                    source : "N7🚫X slam bonus and vulnerable"}
                : Just {team : We, below : false, amount : 150,
                    source : "N7🚫X honours (aces)"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 6940, they : 0} scorepad.totals.total
        test "Scores S2♠-1, S4♥+0h, N2🚫-3h, S6♣+1h" do
            let maybeHands = do
                    level2 <- toLevel 2
                    level4 <- toLevel 4
                    level6 <- toLevel 6
                    tricks5 <- toTricks 5
                    tricks7 <- toTricks 7
                    tricks13 <- toTricks 13
                    let hand1 = newHand
                            # setDeclarer 'S' We
                            # setLevel level2
                            # setSuit Spades
                            # setTricks tricks7
                            # setHonours None
                        hand2 = newHand
                            # setDeclarer 'S' We
                            # setLevel level4
                            # setSuit Hearts
                            # setHonours (Hons We Four)
                        hand3 = newHand
                            # setDeclarer 'N' We
                            # setLevel level2
                            # setSuit NoTrumps
                            # setTricks tricks5
                            # setHonours (Hons They Aces)
                        hand4 = newHand
                            # setDeclarer 'S' We
                            # setLevel level6
                            # setSuit Clubs
                            # setTricks tricks13
                            # setHonours (Hons We Five)
                    pure (hand1 : hand2 : hand3 : hand4 : Nil)
            A.assert "Created hands" $ isJust $ maybeHands
            let hands = unsafePartial $ fromJust $ maybeHands
            A.assert "Scored hands" $ isRight $ traverse scoreHand hands
            let scoredHands =
                    unsafePartial $ fromRight $ traverse scoreHand hands
                scorepad = scoreAll "Rubber Bonus" scoredHands
            A.equal
                ( Just {team : They, below : false, amount : 50,
                    source : "Lost S2♠ with 1 undertrick(s)"}
                : Just {team : We, below : true, amount : 120,
                    source : "Won S4♥"}
                : Nothing
                : Just {team : We, below : false, amount : 100,
                    source : "S4♥ honours (4 out of 5)"}
                : Just {team : They, below : false, amount : 300,
                    source : "Lost N2🚫 with 3 undertrick(s)"}
                : Just {team : They, below : false, amount : 150,
                    source : "N2🚫 honours (aces)"}
                : Just {team : We, below : true, amount : 120,
                    source : "Won S6♣"}
                : Nothing
                : Just {team : We, below : false, amount : 700,
                    source : "Rubber Bonus"}
                : Just {team : We, below : false, amount : 20,
                    source : "S6♣ with 1 overtrick(s)"}
                : Just {team : We, below : false, amount : 750,
                    source : "S6♣ slam bonus and vulnerable"}
                : Just {team : We, below : false, amount : 150,
                    source : "S6♣ honours (5 out of 5)"}
                : Nil
                )
                scorepad.entries
            A.equal {we : false, they : false} scorepad.totals.vulnerable
            A.equal {we : 0, they : 0} scorepad.totals.below
            A.equal {we : 1960, they : 500} scorepad.totals.total
