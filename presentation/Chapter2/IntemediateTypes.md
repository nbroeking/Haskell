Intermediate Types
==================

In the previous section, we learned about basic types such as the primitives
like `Int`, `Integer`, `Bool` etc. We also learned what product types and sum
types are, how to define our own, and how to pattern match on those types. We
also learned about recursive types and finally how to genericize our types and
create _type functions_. There is yet still more to the Haskell type system
than even that! We are all used to the idea of having _polymorphism_. In
Object Oriented Programming, polymorphism looks like class extension to allow
multiple different types to behave in a similar matter, but have different
implementations.

In C++, in addition to the OOP method of polymorphism, it can also use templates
to achieve a level of polymorphism. This is how the STL in C++ works to provide
powerful, polymorphic functions like `find`, or `foreach`.

In Haskell, there are no objects in the OO sense, instead Haskell achieves polymorphism
through the use of _Abstract Data Types_ which are much closer to the template polymorphism
in C++ than object oriented polymorphism. The way this is accomplished is with an
entity know as _typeclasses_. These are denoted by the keyword _class_, but it is important
to understand that these Haskell classes are fundamentally different from C++ or Java
classes. Classes in Haskell are a way of classifying types based on what functions
may operate on them. For example, we will use an example built into Haskell.

```haskell
    class Show t where
        show :: t -> String
```

What this is saying is we define a class of types called `Show` such that all types
in the `Show` class implement the function `show` which has type `t -> String` where
`t` is the type.

That is a bit of a mouthful, so lets look at an instance of this class:

```haskell
    instance Show Bool where
        show True = "True"
        show False = "False"
```

This chunk of code is saying that the type `Bool` is in the class `Show`. Since we
told Haskell that Bool is in the Show class, we need to actually implement what the
show function does for `Bool`s. This is the main way in Haskell to overload functions
based on parameters.

So how do we use typeclases? Typeclasses are used in the _context_ of a type signature.
We may have a type that looks like `(Show a) => a -> b`. This says that this function
will accept any type for `a` as long as that type is in the class `Show` (and therefore
implements the function `show`).

That is well and good, but the question might now become 'How is this different from Java's
toString() method?'. To answer let me give an example of a class called `Addable`:

```haskell
    class Addable a where
        add :: a -> a -> a
        zero = a
```

For a type to be in the class `Addable`, it must implement an add function that takes
two values of the same type and returns their sum. There is one obvious implementation
of this class:

```haskell
    instance Addable Int where
        add = (+)
        zero = 0
```

Here we define that Int is in the class Addable and implement the add function to be the
simple addition operator and 0 as zero. We can define another instance of `Addable` for lists

```haskell
    instance Addable [a] where
        add = (++)
        zero = []
```

This uses list append for the "addition" operator and the empty list as "zero".

Finally, we can create our own data and make it an instance of addable.

```haskell
    data Fn a = Fn (a -> a)
    instance Addable (Fn a) where
        add (Fn f1) (Fn f2) = Fn (f1 . f2)
        zero = Fn id
```

This essentially defines an intsance of addable for functions of the type `a -> a`
(The reason we must wrap it in our own type is aut of the scope for now) using
function composition for the adding function and the identitiy function as
"zero".

What is the point in doing all this you might be wondering? We now have 3 types in
the addable class, and that now gives me the power to build up more powerful and
generic computations using what I know about the add class. For example, I can create
a function called `concat` that takes a list of "Addables" and "sums" them up.

We can define this function as such:

```haskell
    concat :: (Addable a) => [a] -> a
    concat [] = zero
    concat (a:as) = add a (concat as)
```

And tht is the whole implementation of the function. Now we can use this function in a
number of interesting ways:

```haskell
    Prelude> let num = concat [1, 8, 3]
    Prelude> let list = concat [[1, 2, 3], [4, 5, 6]]
    Prelude> let (Fn function) = concat [Fn (+4), Fn (*3), Fn (+1)]
    Prelude> print num
    12
    Prelude> print list
    [1, 2, 3, 4, 5, 6]
    Prelude> print (function 5)
    28
```
(Note: what the function one does is define a list of functions that
the concat function then strings together to create a new function. The
resulting function in this case is `f x = (x + 4) * 3 + 1`

Notice how versitile a function we just made! Our concat function can sum a
list of numbers, it cat flatten a list of lists and it can build computations
from a list of functions! This is an extremely powerful concept and the basis
for great abstractions.
