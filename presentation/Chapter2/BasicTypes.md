Basic Types in Haskell
----------------------
Now that we have been showing you basic haskell we need to show you what types are there so you can actually write useful programs. The Haskell type system is based of the Hindley-Milner type system. Essentially, this means that the whole algebraic representation is based off of research done by Hindley and Milner. We aren't going to dive down into what this means but if your interested there are some great articles on wikipedia. 


Haskell has a number of built in types. To start with, there is,
`Integer`, `Boolean`, `String`, `Lists`, `Tuples` among others.

Lets step through these one by one.

#### Integral Types

`Integers` in Haskell are like Python Integers, unbounded. In Haskell,
Integers may be created with literal numbers.

```haskell
    let myInteger = 5 :: Integer
```

Integers have some built in operators, like addition `+`, subtraction `-`,
multiplication `*` and more. Pretty much everything that you have been using in every other language, ever.

However in Haskell, there are more than just one type of Integer. Not only are
there `Integers` but there is also `Int`. `Int` is like `Integer`, except
it is bounded, like an `int` in Java or C. While it is bounded, it is also
a lot faster, so it is important to use the `Int` type if necessary.

In addition to these two integral types, there are also numbered integers
like `Int16`, `Int32` and `Int64` which are similar to the `int16_t`, `int32_t`,
and `int64_t` types in C.

Finally, there are also `Word` types. These are like the unsigned types in
C and C++.

More or less these aren't any different than what every other language has. Differrent situations require different types of integers.

#### Floating Types

In addition to integral types, there are also Floating types. These types
include `Float` and `Double`. These are exactly like the `float` and `double`
types in other languages. Floats are like integrals, except they also implement
floating division.

#### Bools

Bools in Haskell work just like booleans in Java. They have two constructors,
True and False.


#### Lists

In Haskell, there are list types, denoted by `[a]` where `a` is the type the list holds.
In Haskell, lists like these are implemented as linked lists, as they are in most
other functional programming languages. The implementation as a linked list allows
for very nice abstractions and works well with the purity system in Haskell.

#### Unit

In Haskell, and in many Functional programming languages, there is a notion of a type called
`unit`. This type has exactly one representation and is denoted in Haskell by closing
parenthesis `()`. The unit type in Haskell is extremely important because it encapsulates
the notion of computation without returning a value. This is semantically roughly equivalent to the notion
of `void` in imperative languages like Java; in functional programming, this name is avoided
as `void` has a different meaning; `void` is the type that has no representations and can therefore
never be constructed

###Algebraic Data Types

The Hindley-Milner type-system is called an algebra of types because it defines a set of operators
and values (types) such that when these operators are applied to types new types are returned; just
like addition and multiplication for numbers, and as we'll see there is significant overlap between
type algebra and the algebra you learn in grade school.

In Haskell, all new types are built from "primitive types" like `()`, `Integer`, `Int`, `Float`, etc.
by concatenating them with these different type operators.

#### Pairs

Pairs, or tuples, are a very important concept in Hindley-Milner type systems. It is effectively
analogous to multiplication in school algebra. Tuples are created using the parenthesis:

```Haskell
    let mytuple = (True, False) in ...
```

The type of `mytuple` in the above example is `(Bool, Bool)`, that is, it denotes a pair
of boolean values. If for some reason we want to create our own data structure that has two
booleans and is semantically equivalent to `(Bool, Bool)` we can create such a type this way (in ghci):

```haskell
    Prelude> data TwoBools = Two Bool Bool
```

This is pretty straight forward to read if you know what you are looking at. First is the keyword
`data`. This tells the compiler that we are going to create a new data type. Next, `TwoBools`, is the name
we are going to give to that type. The next part is a little confusing, `Two` is the name of the constructor
we give to the object, and the next types `Bool Bool` are the types of the fields associated with this
constructor. Notice that the fields do not have names. That is because we can reference them using
"Pattern Matching", a concept that will be discussed below.

We now have a type that can carry two `Bool`s, just like the tuple. We yet do not know how to construct it.
We construct one of these TwoBools types by using the constructor. The constructor can be thought of as
just another function that takes two `Bool`s and returns a TwoBools.

```haskell
    Prelude> let mybools = Two True False
```

###### Pattern matching

Many functional programming languages have the concept of "pattern matching" and Haskell is no different. What
is pattern matching? It is a way of decomposing a type into its parts. To pattern match, you simply replace
a variable with the constructor used to create it. For example, I write code like this:

```haskell
    Prelude> let mybools = Two True False
    Prelude> let (Two a b) = mybools
    Prelude> print a
    Prelude> print b
```

This program will print "True" followed by "False". Notice that on line 2, we are doing a pattern match.
At that point in the program, Haskell deconstructs `mybools` and binds `a` and `b` to the original
booleans used to create it.

We can be more specific too. We can provide additional patterns of each of the structures inside the
bigger structure

```haskell
    Prelude> let mybools = Two True False
    Prelude> let (Two True a) = mybools
    Prelude> print a
```

This now prints just "False". Now a keen eye might question what would happen if
we tried to deconstruct with `(Two False a)`. Lets give it a try:

```haskell
    Prelude> let (Two False a) = mybools
    Prelude> a
    *** Exception: <interactive>:9:5-27: Irrefutable pattern failed for pattern (Two False a)
```

What happened there?! Well, we constructed mybools using the constructor `Two True False`,
but yet we are trying to deconstruct it with that first value equal to `False`. Haskell is
complaining then that the pattern supplied does not match the pattern used for construction.

#### Sum Types

We have now learned about pairs and how to create a new type from another type by using
what is called a "product" combination, which is a fancy way of saying we paired the types
together in a way that is similar to mathematical multiplication. Don't worry yet about
how it is similar, because we still must learn about "sum types".

Sum types are one of the corner stones of the Hindley-Milner type system. It is roughly equivalent
to mathematical addition. You take two types and you can either have one or the other, but
not both. This is in contrast to product types (pairs) where, in our previous example, you must
have both bools present to have a TwoBool. This is not the case with sum types.

This is achieved by the use of _multiple_ constructors, each with their own, distinct substructures.

Let us extend our TwoBools data type to have another constructor:

```haskell
    Prelude> data TwoBools = Two Bool Bool | One Bool
```

With this syntax, we have defined another constructor called `one` that only accepts a single Bool
as an argument. Now we may construct a `TwoBools` two different ways.

```haskell
    Prelude> let mybool1 = Two True False
    Prelude> let mybool2 = One False
```

Sum types are very confusing to imperative programmers because it means that a single type
may have very different fields based on what constructor was used to create it. There is
nothing quite like it in other languages. The closest analogue we can come to is a struct
like the following in C++:

```c
    enum TwoBoolsType {
        TWO, ONE
    };
    struct TwoBools {
        TwoBoolsType type;
        union {
            struct {
                /* if type == TWO */
                bool a;
                bool b;
            };
            struct {
                /* if type == ONE */
                bool c;
            };
        }
    };
```

###### Pattern Matching

Since we now can have very different representations of the same Type, it is no
longer sufficient no use the naive pattern matching above. Instead, we must
branch our flow of control using a `case` statement. What this statement does
is allow us to supply multiple patterns and allow us to execute different code
depending on the branch that was matched.

```haskell
    Prelude> let myboolone = One True
    Prelude> let mybooltwo = Two False False
    Prelude> :{
    Prelude|    case myboolone of
    Prelude|       (One a) -> putStrLn ("This was created using (One " ++ show a ++ ")")
    Prelude|       (Two a b) -> putStrLn ("This was created using (Two " ++ show a ++ " " ++ show b ++ ")")
    Prelude> :}
```

This prints "This was created using (One True)" because the first pattern to match was `(One a)` so the co
likewise, running

```haskell
    Prelude> let myboolone = One True
    Prelude> let mybooltwo = Two False False
    Prelude> :{
    Prelude|    case mybooltwo of
    Prelude|       (One a) -> putStrLn ("This was created using (One " ++ show a ++ ")")
    Prelude|       (Two a b) -> putStrLn ("This was created using (Two " ++ show a ++ " " ++ show b ++ ")")
    Prelude| :}
```

Would print "This was created using (Two False False)". This is because the flow of
control passed over the pattern `(One a)` because it did not match and went straight on to
the second one.


### Functions

Functions in Haskell are especially interesting and thus far, we have be glazing over
functions in good faith, but for no longer. Functions are a type just like any other
in Haskell. We denote a function with the arrow `a -> b`. This means a function that
takes a type of `a` and returns a type of `b`.

The keen mind might ask, "what if I have multiple parameters? How do I show the type then?"
This is really a trick question, because there is no such type! That is right, every function
in haskell has exactly one parameter and exactly one return type! How might this be possible
you might ask. Well, thanks to a notion known as currying this property of one return and one
parameter is not only possible, but extremely elegant.

In C, a division function has the type `double(*)(double,double)`, meaning a function that
takes two doubles and returns a third. In haskell such a function may have two representations,
either `(double, double) -> double` which means a function that takes a pair of doubles and
returns a double, or the far more common and flexible version `double -> (double -> double)`.
This is saying `div` has the type of a function that takes a double and returns a function
that takes another double and returns the third double.

Since the `->` operator is right-associative, the parenthesis may be dropped and the
type then becomes `double -> double -> double`. This is an extremely powerful notion
of a function because it allows for partial application; the notion that you can
provide some of the arguments to a function up front and apply the rest somewhere else.

Functions are paramount in Haskell; after all Haskell is a functional programming language.
Virtually everything is a function, even the expression

```haskell
    let a = 5
```

Isn't assigning a to 5, it is assigning a to a function that takes no parameters and returns 5.
This is possible through the mechanism of lazy evaluation.

### Recursive Types

To build some very complex data structures in Haskell, we need to have a notion of recursive
types. This is easily possible in Haskell, we can just define a recursive type by using
it in the `data` section.

```haskell
    Prelude> data MyData = MyConstructor MyData
```

This is not a really useful type if you actually look at it because it just infinitely
self-references. This idea can be used to create integral types like lists. We can
create a copy of the Haskell list for int types with the following:

```haskell
    Prelude> data MyIntList = Nil | Cons Int MyIntList
```

What is happening here is that I have two constructors, one called `Nil` for
the end of the list and another called `Cons` that has a value and a link to
the next value.

### Generic Types

You may be wondering now how does one create a list that not only contains `Int`s
but can contain any type? This is the world of generic programming. It is an important
concept in other languages as well, like Java and C++. In C++ these are called templates
and in Java they are called Generics. In C++ we might write a class that looks
something like this:

```c++
    template<class T>
    class List { ... }
```

in Haskell, we can write a semantically equivalent, generic version of our IntList as

```haskell
    data List t = Nil | Cons t List
```

Here, the little 't' is the generic. It means we can apply any type as an argument
to the List type and it uses that in all instances. In functional programming,
this can be considered to be a type function. `List` by itself is not a type, it
takes an argument so while `List` is not a type, `List Int` is a type. We use this
notion to say `List` has the _kind_ `* -> *`.


<!---
At the bottom of every page we need a next and previous button 
-->
<hr>
[Home](../../README.md) | [Back](../Chapter1/CompilingHaskell.md) | [Next](IntemediateTypes.md)
