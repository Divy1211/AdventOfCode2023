module Day7.Hand2 (Card (..), fromChar, Hand (..), fromStr) where

import qualified Data.Map as Map
-- todo: figure out how to override the enums/fns from Hand1 instead of duplicating all this code

data Card = JACK
          | TWO
          | THREE
          | FOUR
          | FIVE
          | SIX
          | SEVEN
          | EIGHT
          | NINE
          | TEN
          | QUEEN
          | KING
          | ACE
          deriving (Enum, Eq, Ord, Show)

fromChar :: Char -> Card
fromChar 'A' = ACE
fromChar 'K' = KING
fromChar 'Q' = QUEEN
fromChar 'J' = JACK
fromChar 'T' = TEN
fromChar '9' = NINE
fromChar '8' = EIGHT
fromChar '7' = SEVEN
fromChar '6' = SIX
fromChar '5' = FIVE
fromChar '4' = FOUR
fromChar '3' = THREE
fromChar '2' = TWO

data Hand = Hand {
    c1 :: Card,
    c2 :: Card,
    c3 :: Card,
    c4 :: Card,
    c5 :: Card
} deriving (Eq, Show)

instance Ord Hand where
    h1 <= h2 =
        let [t1, t2] = map determineHandType [h1, h2] in
        if t1 /= t2 then t1 < t2 else
        let fields = [c1, c2, c3, c4, c5] in
        let v1 = map ($ h1) fields in
        let v2 = map ($ h2) fields in
        v1 <= v2

fromStr :: String -> Hand
fromStr str = do
    let [ca1, ca2, ca3, ca4, ca5] = map fromChar (take 5 (str ++ repeat '2'))
    Hand{c1 = ca1, c2 = ca2, c3 = ca3, c4 = ca4, c5 = ca5}

data HandType = DISTINCT
              | PAIRx1
              | PAIRx2
              | TRIPLE
              | TRIPLEwPAIR
              | QUAD
              | QUINT
              deriving (Enum, Eq, Ord, Show)

determineHandType :: Hand -> HandType
determineHandType h = do
    let cards = map ($ h) [c1, c2, c3, c4, c5]
    let counts = Map.fromListWith (+) (zip cards (repeat 1))

    let handType =
            Map.foldl (\type' count ->
                case count of
                    1 -> type'
                    2 | type' < TRIPLEwPAIR -> succ type'
                    2 -> type'
                    3 | type' == PAIRx1 -> TRIPLEwPAIR
                    3 -> TRIPLE
                    4 -> QUAD
                    5 -> QUINT
            ) DISTINCT

    let (numJs, counts') = Map.updateLookupWithKey (\_ _ -> Nothing) JACK counts
    case numJs of
        Nothing -> handType counts
        Just 5 -> QUINT
        Just 4 -> QUINT
        Just j -> do
            let (maxV, card) = maximum $ map (\(x, y) -> (y, x)) $ Map.toList counts'
            handType (Map.update (\_ -> Just (maxV + j)) card counts')