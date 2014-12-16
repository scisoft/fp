# Functional programming

## Jonas Juselius <jonas.juselius@uit.no>

HPC@UiT

---

layout: false

## What is Functional Programming
* Invented in the 30ies (Alonzo Church)
* Not popularized due to limited computers
* Functions are *first class*
    * Data and functions are equivalent
    * Lambda functions
    * Higher-order functions
    * Function composition
    * Partial function application

* Functions are pure
* Immutable data: no state, no variables
* Advanced type systems

---

## Languages
* Haskell: Pure functional, immutable, lazy, advanced types
* Scala: Functional, object oriented running on JVM
* Clojure: Modern, concurrent LISP on top of JVM
* F#: Functional, object oriented on top of CLR
* Python: lambda, HOFs, map, zip, filter, reduce
* JavaScript can emulate functional programming with HOFs

---

## Haskell
* Purely functional language
* Strongly typed (and no ma, you ain't seen strong)
* Lazy by default
* Elegant syntax
* Efficient
* "If it compiles, it works!" (often)

<img src="{{base}}/img/compiles_and_works.jpg" style="width: 50%;"/>

---

## Function composition

> Composition is *the* way to reduce complexity

```{Python}
    print(sin(float('2')))
    a = f(g(h(x)))
```

```{sh}
    $ cat file.txt | sort
    $ cat file.txt | rev | head -2
    $ cat file.txt | tr a-z A-Z | sed 's/$/!!!/'
```

```{Haskell}
    a x = take 2 . rev . sort $ x
    a' = take 2 . rev . sort
```

> This is why we don't use parens in Haskell

---

## Purity
* Pure functions has no notion of state: They take input values and return
  values
* Given the same input, a pure function *always* returns the same value!
  Function calls can be optimized away!
* Pure function == data!
* Purity is key to equational reasoning

<img src="{{base}}/img/bugbarrier.jpg" style="width: 40%;"/>

---

## Currying
* Partial application of functions
* All functions take one or zero arguments and return a function

```{Haskell}
    add :: Int -> Int -> Int
    add x y = x + y

    add42 :: Int -> Int
    add42 = add 42

    mulf2 :: (Int -> Int) -> Int -> Int
    mulf2 f x = 2 * f x

    main = do
        print $ mulf2 (add 42) 5
        print $ mulf2 add42 5
```

---

## Looping
* No state -> no loops: Recursion  and tail recursion
* Loops are hard to understand (but recursion is worse)
* Loops are not declarative
* map, filter and fold: Looping with style!

---

## Concurrency
Concurrency is nearly trivial

```{Haskell}
import Data.Vector as V
import Control.Parallel.Strategies

main :: IO ()
main = do
    let n = 1000.0
        a = fromList [1.0..n]
        b = V.map fun a `using` parTraversable rseq
    print $ dotp a b

fun :: Float -> Float
fun x = y + 1.0 / exp y
    where
        y = x / sqrt x

dotp :: Vector Float -> Vector Float -> Float
dotp a b = V.sum (V.zipWith (*) a b `using` parTraversable rseq)
```

---

## Real life
<img src="{{base}}/img/csharp_vs_fsharp.png" style="width: 100%;"/>

## Summary of advantages
* Reduced complexity
* Easier to reason about code
* Easier testing
* Easier parallelism and concurrency
* Higher level abstractions
* Less boiler plate, shorter code

<img src="{{base}}/img/haskell.png" style="width: 100%;"/>

---

## Fortran example

```{Fortran}
program fp
    implicit none
    integer :: i
    real, dimension(:,:), allocatable :: x

    x = qeq(life(5), 4.0, 2.0)
    print *, x
contains
   pure function life(a) result(b)
      integer, intent(in) :: a
      real, dimension(:,:), allocatable :: b

      allocate(b(a,a))
      b = 42.0
   end function
   elemental function qeq(a, b, c) result(y)
      real, intent(in) :: a, b, c
      real :: y

      y = -b + sqrt(b**2 + 4*a*c)/a/2.0
   end function
end program
```

---

## What can I do?
* In fortran, write *pure* and *elemental* functions when possible
* In C++ use shared_ptr and/or smart_ptr
* Write modular code
* Don't write objects when a function will do
* Write short functions which do one thing and do it well
* Avoid global variables, including module and object state
* Keep the IO layer connected and thin
* Don't reuse variables


