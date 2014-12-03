.. role:: cover

============================================
:cover:`Functional programming`
============================================

.. class:: cover

    ::

        Jonas Juselius <jonas.juselius@uit.no>

        HPC@UiT

.. raw:: pdf

   SetPageCounter 0
   Transition Dissolve 1
   PageBreak oneColumn

What is Functional Programming
----------------------------------------------------
* Functions are *first class*
* Functions are pure
* Data and functions are equivalent
* Immutable data: no state, no variables
* Lambda functions
* Higher-order functions (HOF)
* Function composition
* Advanced type systems

Why FP?
----------------------------------------------------
* Reduce complexity
* Easier to reason about code
* Easier testing
* Easier parallelism and concurrency
* Higher level abstractions
* Less boiler plate, shorter code
* Easy to create domain-specific languages

Foundations of functional programming
---------------------------------------------------
* Invented in the 30ies (Church)
* Microsoft bought half the core Haskell team
* Investment banks bought the rest

::

    Imperative programming: Do as you please
    Functional programming: Stay on the narrow path

Langugaes
---------------------------------------------------
* Haskell: Pure functional, immutable, lazy, advanced types
* Scala: Functional, object oriented running on JVM
* Clojure: Modern, concurrent LISP on top of JVM
* F#: Functional, object oriented on top of CLR
* Python: lambda, HOFs, map, zip, filter, reduce
* JavaScript can emulate functional programming with HOFs

Haskell
---------------------------------------------------
* Purely functional language
* Strongly typed (and no ma, you ain't seen strong)
* Lazy by default
* Elegant syntax
* Efficient
* "If it compiles, it works!" (often)

Composability
---------------------------------------------------

* Function composition (you know this already!)
* Unix shell and pipes:

.. code:: sh

    $ cat file.txt | sort
    $ cat file.txt | rev | head -2
    $ cat file.txt | tr a-z A-Z | sed 's/$/!!!/'

* Composition is *the* way to reduce complexity
* The power of function composition: Moniods
* t = n + m % 12

Purity
---------------------------------------------------
* Pure functions has no notion of state: They take input values and return
  values
* Given the same input, a pure function *always* returns the same value! Thus,
  function calls can be optimized away!
* Pure function == data!
* Purity is key to equational reasoning

Currying
---------------------------------------------------
* Partial application of functions
* All functions take one or zero arguments and return a function

.. code:: haskell

    add :: Int -> Int -> Int
    add x y = x + y

    add42 :: Int -> Int
    add42 = add 42

    mulf2 :: (Int -> Int) -> Int -> Int
    mulf2 f x = 2 * f x

    main = do
        print $ mulf2 (add 42) 5
        print $ mulf2 add42 5

Function composition
---------------------------------------------------
* You already know and use this every day:

.. code:: python

    print(sin(float('2')))
    a = f(g(h(x)))

.. code:: haskell

    a = f . g . h $ x
    b = f . g . h
    c = (add 55.0 . addStrToInt "hej" . mul 8) 42

* This is why we don't use parens in Haskell
* Composition is monoidal
* Function composition is *the* way to reduce complexity!

.. include:: composition.hs
    :code: haskell

Looping
---------------------------------------------------
* No state -> no loops: Recursion  and tail recursion
* Loops are hard to understand (but recursion is worse)
* Loops are not declarative
* map, filter and fold: Looping with style!

Lessons from the lambda crowd
---------------------------------------------------
* Keep the IO layer connected and thin
* Write elemental or pure functions
* Don't reuse variables
* Avoid state, global and local

