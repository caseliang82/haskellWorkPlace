data Season = Winter | Spring | Summer | Fall
              deriving (Eq, Ord, Show, Enum)

next :: Season -> Season
next x  =  toEnum ((fromEnum x + 1) `mod` 4)

warm :: Season -> Bool
warm x  =  x `elem` [Spring .. Fall]
