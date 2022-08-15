{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

import EasyMode
import qualified Prelude as P

inc = (+ 1)
double = (* 2)

data Student = Student {name :: Text, score :: Integer} deriving (P.Show, Eq, Ord)

main :: IO ()
main = do
    print (1)
    print (1 . inc)
    print (1 . double . inc)
    print (1 . inc . double)
    print (2 . ((+ 1) <<< (+ 2)))

    let s1 = Student "alice" 90
    let s2 = Student "bob" 60
    let students = [s1, s2, s1{score = 30}]
    print (students . groupBy (.name) . toPairs . map (second length))
