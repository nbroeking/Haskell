Basic Types in Haskell
======================

Haskell has a number of built in types. To start with, there is,
`Integer`, `Boolean`, `String`, `Lists`, `Tuples` among others.

### Integral Types

`Integers` in Haskell are like Python Integers, unbounded. In Haskell,
Integers may be created with literal numebrs.

```haskell
    let myInteger = 5 :: Integer
```

Integers have some built in operators, like addition `+`, subtraction `-`,
multiplication `*` and more.

In Haskell, there are some different types of integers. Not only are
there `Integers` but there is also `Int`. `Int` is like `Integer`, except
it is bounded, like an `int` in Java or C. While it is bounded, it is also
a lot faster, so it is important to use the `Int` type if necessary.

In addition to these two integral types, there are also numbered integers
like `Int16`, `Int32` and `Int64` which are similar to the `int16_t`, `int32_t`,
and `int64_t` types in C.

Finally, there are also `Word` types. These are like the unsigned types in
C and C++. 

### Floating Types

In addition to integral types, there are also Floating types. These types
include `Float` and `Double`. These are exactly like the `float` and `double`
types in other languages. Floats are like integrals, except they also implement
floating division.

### Bools

Bools in Haskell work just like booleans in Java. They have two constructors,
True and False.


### Lists

In Haskell, there are list types, denoted by `[a]` where `a` is the type the list holds.
In Haskell, lists like these are implemented as linked lists, as they are in most
other functional programming languages. The implementation as a linked list allows
for very nice abstractions and works well with the purity system in Haskell.
