EasyMode
========

A Prelude alternative aims on **easy to use**

Features
--------

- Use `Text` instead of `String` (i.e. `[Chat]`) by default.
- Use `Integer` instead of `Int` by default.
- All Partial Functions are marked as `Partial =>` so you will not miss the stack trace.
- All functions follows the [**LISO**](./doc/LISO.md) (Loose Input and Strict Output) Principle.
- There's NO need to consider the global uniqueness of **field names** anymore.
- Easy-to-use type casting functions.
- Accountable Error Handling ([**AEH**](./doc/AEH.md)).

Usage
-----

EasyMode depends on several language extensions, see the doc about [preparation](./doc/prepare.md).

Now we start coding. First, you need to import EasyMode instead of Prelude,

```haskell
import EasyMode
```

You can freely define field names without considering their global uniqueness,

```haskell
data Student = Student {name :: Text, score :: Integer} deriving (Show, Eq, Ord)
data School = School {name :: Text, student_number :: Integer} deriving (Show, Eq, Ord)
```

Most of the Haskell syntax you are familiar with is supported here,

```haskell
inc = (+ 1)
double = (* 2)

let s1 = Student "alice" 90
let s2 = s1{name = "bob"}

let sc1 = School "school1" 100
let sc2 = s1{name = "school2"}
```

Except for `.` which is now used for field access,

```haskell
test1 = do
    print s2.name
    print s2.score

    print sc2.name
    print sc2.student_number
```

And `.` is also used for flipped function application, which is very similar to field access,

```haskell
test2 = do
    print (1 . inc)
    print (1 . double . inc)
    print (double 1 . inc)
    print (1 . inc . double)
```

If you want `.` back, use `<<<` instead, and `>>>` is also available as a flipped version,

```haskell
test3 = do
    print (1 . (inc >>> double))
    print (1 . (double <<< inc))
    print (1 . ((+ 1) >>> (* 2)))
```

there is no special handling here because a function is also an instance of Arrow.

And there are also handy tool functions like `groubBy` and `toPairs`

```haskell
test3 = do
    let students = [s1, s2, s1{score = 30}]
    print (students . groupBy (.name) . toPairs . map (second length))

    let schools = [sc1, sc2, sc1{student_number = 50}]
    print (schools . groupBy (.name) . toPairs . map (second (sum <<< map (.student_number))))
```

Type casting is easy here, just use `as*` to constraint type of literal and `to*` to cast it to what you want

```haskell
    print (("1" . asText . toInteger) + (2 . asInteger))
```
