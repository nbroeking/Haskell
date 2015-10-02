Basic (Haskell) Category Theory
=====================

Haskell is very much based around a branch of mathematics known as
category theory. Even though Haskell is based around category theory,
it is not necessary to learn it to get the basics of the language
down. Most tutorials will just gloss over the details of constructs
like functors, monads and applicatives until later in the tutorial.
This is that time.

Category theory is a very advanced and abstract branch of mathematics,
so while it is useful to know formal category theory, even the basics
would take too long to describe, so in this section we will just describe
an introduction to category theory purely in the context of Haskell.

In the last chapter we learned about typeclasses and how they define
behavior for a class of types. We learned specifically about one important
typeclass called `Monoid`. Typeclasses are the crux of what is about to
come.

Functors
--------

Functors are an important basis of the categories. Functors are not the same as functions.
Functors are defined by the typeclass:

```haskell
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
```

You can think of a functor as a box with a type in it. Other than that, the box is opaque,
meaning there is no distinct way of obtaining the value from the box in a strict sense,
however you can apply a function to value inside the box and get a new box with the result
of that value inside of it.

There are many exmples of functors in Haskell as it is one of the more important typeclasses
in the language. One example of such a type is a basic list. You may be familiar with
the `map` function from other functional languages. This is the implementation of `fmap` 
for lists. Here would be the implementation:

```haskell
    instance Functor [a] where
        fmap _ [] = []
        fmap f (a:as) = f a : fmap f as
```

This function simply applys another function to all the values in a list.

Another example of a functor in Haskell is Haskell's `Maybe` type. This type
is like Scala's option type. It has two constructors `Just x` and `Nothing`.
In other words, it is a way of implementing null in safe functional languages.

```haskell
    data Maybe a = Just a | Nothing
```

So we can write an implementation of functor for the `Maybe` type.

```haskell
    instance Functor Maybe where
        fmap _ Nothing = Nothing
        fmap f (Just x) = Just (f x)
```

We can even define a function as being a Functor.

```haskell
    instance Functor ((->) a) where
        fmap f1 f2 = f1 . f2
```

This is just using the function composition operator to create the mapping.



<!---
At the bottom of every page we need a next and previous button 
-->
<hr>
[Home](../README.md) | [Back](./presentation/IntemediateTypes.md) | [Next](../Conclusion.md)