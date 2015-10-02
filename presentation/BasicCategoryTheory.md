Basic (Haskell) Category Theory
--------------------------------

Haskell is very much based around a branch of mathematics known as
category theory. Even though Haskell is based around category theory,
it is not necessary to learn it to get the basics of the language
down. Most tutorials will just gloss over the details of constructs
like functors, monads and applicative until later in the tutorial.
This is that time.

Category theory is a very advanced and abstract branch of mathematics,
so while it is useful to know formal category theory, even the basics
would take too long to describe, so in this section we will just describe
an introduction to category theory purely in the context of Haskell.

In the last chapter we learned about typeclasses and how they define
behavior for a class of types. We learned specifically about one important
typeclass called `Monoid`. Typeclasses are the crux of what is about to
come.

####Functors


Functors are an important basis of the categories. Functors are not the same as functions.
Functors are defined by the typeclass:

```Haskell
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
```

You can think of a functor as a box with a type in it. Other than that, the box is opaque,
meaning there is no distinct way of obtaining the value from the box in a strict sense,
however you can apply a function to value inside the box and get a new box with the result
of that value inside of it.

There are many examples of functors in Haskell as it is one of the more important typeclasses
in the language. One example of such a type is a basic list. You may be familiar with
the `map` function from other functional languages. This is the implementation of `fmap` 
for lists. Here would be the implementation:

```Haskell
    instance Functor [a] where
        fmap _ [] = []
        fmap f (a:as) = f a : fmap f as
```

This function simply applies another function to all the values in a list.

Another example of a functor in Haskell is Haskell's `Maybe` type. This type
is like Scala's option type. It has two constructors `Just x` and `Nothing`.
In other words, it is a way of implementing null in safe functional languages.

```Haskell
    data Maybe a = Just a | Nothing
```

So we can write an implementation of functor for the `Maybe` type.

```Haskell
    instance Functor Maybe where
        fmap _ Nothing = Nothing
        fmap f (Just x) = Just (f x)
```

We can even define a function as being a Functor.

```Haskell
    instance Functor ((->) a) where
        fmap f1 f2 = f1 . f2
```

This is just using the function composition operator to create the mapping.


####Applicative


Applicative's are a middle-child of sorts between functors and Monads. The
typeclass for these applicatives is:

```Haskell
    class (Functor f) => Applicative f where
        (<*>) :: f (a -> b) -> f a -> f b
        pure :: a -> f a
```

The strange `(Functor f) =>` bit is just saying that for a type to be an
Applicative, then it must also be a Functor. In addition to the
`fmap` function, it also adds the `(<*>)` operator and the `pure` function.

The `(<*>)` operator just takes a function wrapped in an applicative and allows
the user to apply an argument wrapped in the same applicative and get back a result
wrapped in the same applicative.

The `pure` function takes a pure value and wraps it in an applicative. Applicatives
are more powerful than functors. From any applicative, we can implement fmap as

```Haskell
    fmap :: (Applicative f) => (a -> b) -> f a -> f b
    fmap fn arg = pure fn <*> arg
```

However, it is impossible to implement either `(<*>)` or `pure` with just `fmap`.


####Monads

Monads are the most powerful abstraction of the three we have discussed. The typeclass
looks like:

```Haskell
    class (Applicative m) => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a
```

Monads are incredibly important in Haskell. They even have special syntax for dealing
with them. This is what the famous `do` block in Haskell does behind the scenes.

For something to be a Monad, it must first be an applicative and also implement the
`bind` operator.

The bind function is similar to `fmap`, especially when the first two parameters are
flipped:

```Haskell
    fmap :: (a -> b) -> m a -> m b
    flippedBind :: (a -> m b) -> m a -> m b
```

the only difference of course is that the function applied does _not_ have to return
a pure value, it can return an impure value. This is where the true power of monads
comes to bear. It is because using this, we can sequence our steps. The `Maybe` type
is a monad. It defines the bind function as this:

```Haskell
    instance Monad Maybe where
        (>>=) Nothing _ = Nothing 
        (>>=) (Just x) fn = fn x
        return = Just
```

This is very useful for detecting errors and handling "null" values. For example
if we have some program that tries to read data from a map, we can sequence this
with

```Haskell
    import Data.Map

    data Person = Person {
        age :: String,
        occupation :: String,
        hairColor :: String,
    }
    
    jsonToMaybePerson :: Map String String -> Maybe Person
    jsonToMaybePerson dat =
        lookup "age" dat >>= \ageStr ->
        lookup "occupation" dat >>= \occupation ->
        lookup "hairColor" dat >>= \hair ->
        Just (Person (read ageStr) occupation hair)
```

The `lookup` function returns `Nothing` if that key is not in the map.
We can make this function more terse by using haskell's do notation:

```Haskell
    jsonToMaybePerson dat = do
        ageStr <- lookup "age"  dat
        occupation <- lookup "occupation" dat
        hair <- lookup "hairColor"
        Just (Person ageStr occupation hair)
```

In fact, we can make this extremely short by using some of the functions
from the previous type classes

```Haskell
jsonToMaybePerson dat = 
    (fmap Person (lookup "age" dat)) <*>
    lookup "occupation" dat <*>
    lookup "hairColor"
```

In Java, the way to write this function is something like

```Haskell
    public Person jsonToMaybePerson(Map<String, String> blob) {
        String age = blob.get("age");
        String occupation = blob.get("occupation");
        String hair = blob.get("hair");

        if(age == null || occupation == null || hair == null) [
            return null;
        }

        return new Person(age, occupation, hair)
    }
```


<!---
At the bottom of every page we need a next and previous button 
-->
<hr>
[Home](../README.md) | [Back](IntermediateTypes.md) | [Next](Conclustion.md)