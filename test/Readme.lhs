First, you need to import EasyMode instead of Prelude

> import EasyMode

You can freely define field names without considering their global uniqueness

> data Student = Student {name :: Text, score :: Integer} deriving (Show, Eq, Ord)
> data School = School {name :: Text, student_number :: Integer} deriving (Show, Eq, Ord)

Most of the Haskell syntax you are familiar with is supported here

> inc = (+ 1)
> double = (* 2)
>
> s1 = Student "alice" 90
>
> sc1 = School "school1" 100

Except for `.` which is now used for field access

> test1 = do
>     print s1.name
>     print s1.score
>
>     print sc1.name
>     print sc1.student_number

And `.` is also used for flipped function application, which is very similar to field access

> test2 = do
>     print (1 . inc)
>     print (1 . double . inc)
>     print (double 1 . inc)
>     print (1 . inc . double)

If you want `.` back, use `<<<` instead, and `>>>` is also available as a flipped version

> test3 = do
>     print (1 . (inc >>> double))
>     print (1 . (double <<< inc))
>     print (1 . ((+ 1) >>> (* 2)))

there is no special handling here because a function is also an instance of Arrow.

And there are also handy tool functions like `groubBy` and `toPairs`

> test4 = do
>     let students = [s1, Student "bob" 60, s1{score = 99}]
>     print (students . groupBy (.name) . toPairs . map (second length))
>
>     let schools = [sc1, School "school2" 100, sc1{student_number = 50}]
>     print (schools . groupBy (.name) . toPairs . map (second (sum <<< map (.student_number))))

Type casting is easy here, just use `as*` to constraint type of literal and `to*` to cast it to what you want

>     print (("1" . asText . toInteger) + (2 . asInteger))

\ignore{
\begin{code}

  main :: IO ()
  main = test1 >> test2 >> test3 >> test4

\end{code}
}
