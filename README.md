EasyMode
========

A Prelude alternative aims on **easy to use**

Features
--------

- Make it easy to Use `Text` instead of `String` by default, but you can still use both if you want.
- Make it easy to Use `Integer` instead of `Int` by default, but you can still use both if you want.
- Easy-to-use **type casting** tool functions (e.g. `toText`, `toPairs`, `toMap`), including Python-style functions (e.g. `int`, `float`, `chr`, `ord`).
- There's NO need to consider the global uniqueness of **field names** anymore.
- All Partial Functions are marked as `Partial =>` so you will not miss the stack trace.
- All functions follows the [**LISO**](./doc/LISO.md) (Loose Input and Strict Output) Principle.
- Bounded Accountability Error Handling ([**BAEH**](./doc/BAEH.md)).

Usage
-----

EasyMode depends on several language extensions, see the doc about [preparation](./doc/prepare.md).

Now we start coding.

```lhaskell

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
```
