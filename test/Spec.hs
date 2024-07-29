import EasyMode
import GHC.Real (fromIntegral)
import Data.String (IsString)

inc = (+ 1)
double = (* 2)

data Student = Student {name :: Text, score :: Integer} deriving (Show, Eq, Ord)
data School = School {name :: Text, student_number :: Integer} deriving (Show, Eq, Ord)

main :: IO ()
main = do
    print 1
    print (1 . inc)
    print (1 . double . inc)
    print (double 1 . inc)

    print (1 . inc . double)
    print (1 . (inc >>> double))
    print (1 . (double <<< inc))
    print (1 . ((+ 1) >>> (* 2)))

    let s1 = Student "alice" 90
    let s2 = Student "bob" 60
    let students = [s1, s2, s1{score = 30}]
    print (students . groupBy (.name) . toPairs . map (second length))

    let sc1 = School "school1" 100
    let sc2 = School "school2" 200
    let schools = [sc1, sc2, sc1{student_number = 50}]
    print (schools . groupBy (.name) . toPairs . map (second (sum <<< map (.student_number))))

    print (("1" . asText . toInteger) + (2 . asInteger))
