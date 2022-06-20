{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}

import EasyMode
import GHC.Show (Show)

inc = (+1)
double = (*2)

data Student = Student {name :: Text, score :: Integer} deriving (Show, Eq, Ord)

main :: IO ()
main = do
    print(1)
    print(1 . inc)
    print(1 . double . inc)
    print(1 . inc . double)
    print(2 . ((+1) <<< (+2)))

    let s1 = Student "alice" 90
    let s2 = Student "bob" 60
    let students = [s1, s2, s1 { score = 30 } ]
    print(students . groupBy (.name) . toPairs . map (second length))

