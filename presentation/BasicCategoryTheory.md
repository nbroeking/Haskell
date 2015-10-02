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


Applicative
-----------

Applicatives are a middle-child of sorts between functors and Monads. The
typeclass for these applicatives is:

```haskell
    class (Functor f) => Applicative f where
        (<*>) :: f (a -> b) -> f a -> f b
        pure :: a -> f a
```

The strage `(Functor f) =>` bit is just saying that for a type to be an
Applicative, then it must also be a Functor. In addition to the
`fmap` function, it also adds the `(<*>)` operator and the `pure` function.

The `(<*>)` operator just takes a function wrapped in an applicative and allows
teh user to apply an argument wrapped in the same applicative and get back a result
wrapped in the same applicative.

The `pure` function takes a pure value and wraps it in an applicative. Applicatives
are more powerful than functors. From any applicative, we can implement fmap as

```haskell
    fmap :: (Applicative f) => (a -> b) -> f a -> f b
    fmap fn arg = pure fn <*> arg
```

However, it is impossible to implement either `(<*>)` or `pure` with just `fmap`.


Monads
------

Monads are the most powerful abstraction of the three we have discussed. The typeclass
looks like:

```haskell
    class (Applicative m) => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a
```

Monads are incredibly important in Haskell. They even have special syntax for dealing
with them. This is what the famous `do` block in haskell does behind the scenes.

For something to be a Monad, it must first be an applicaitve and also implement the
`bind` operator.

The bind function is similar to `fmap`, especially when the first two parameters are
flipped:

```haskell
    fmap :: (a -> b) -> m a -> m b
    flippedBind :: (a -> m b) -> m a -> m b
```

the only difference of course is that the function applied does _not_ have to return
a pure value, it can return an impure value. This is where the true power of monads
comes to bear. It is because using this, we can sequence our steps. The `Maybe` type
is a monad. It defines the bind function as this:

```haskell
    instance Monad Maybe where
        (>>=) Nothing _ = Nothing 
        (>>=) (Just x) fn = fn x
        return = Just
```

This is very useful for detecting errors and handling "null" values. For example
if we have some program that tries to read data from a map, we can sequence this
with

```haskell
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
        return (Person (read ageStr) occupation hair)
```

The `lookup` function returs `Nothing` if that key is not in the map.
We can make this function more terse by using haskell's do notation:

```haskell
    jsonToMaybePerson dat = do
        ageStr <- lookup "age"  dat
        occupation <- lookup "occupation" dat
        hair <- lookup "hair"
        return (Person ageStr occupation hair)
```

In fact, we can make this extremely short by using some of the functions
from the previous typeclasses

```haskell
jsonToMaybePerson dat = 
    (fmap Person (lookup "age" dat)) <*>
    lookup "occupation" dat <*>
    lookup "hair"
```

In Java, the way to write this function is something like

```java
    public Person jsonToMaybePerson(Map<String, String> blob) {
        String age = blob.get("age");
        String occupation = blob.get("occupation");
        String hair = blob.get("hair");

        if(age == null || occupation == null || hair == null) {
            return null;
        }

        return new Person(age, occupation, hair)
    }
```

We are not yet done. With just these basic functions implemented, so much
is possible. In fact, Haskell provides a library called `Control.Monad` that
has many of these very useful abstractions provided. One of which is called
`sequence :: (Monad m) => [m a] -> m [a]` and is implemented as

```haskell
sequence [] = return []
sequence (ma:mas) = do
    a <- ma
    as <- mas
    return (a:as)
```

so we can rewrite the above even as this

```haskell
    jsonToMaybePerson dat = do
        [age, occupation, hair] <- sequence( map (\s -> Map.lookup s dat) ["age", "occupation", "hair"] )
        return (Person age occupation hair)
```

#### State Monads

Monads are very useful for carrying around state in a pure way. Imagine we want
to carry around some state `U`. In a pure functional language we need to send
in the state to a function and that function must return a new, mutated state
along with what it is actually returning.

```haskell
    stateModifyingFunction :: U -> b -> (U, a)
```

this function takes some state, U, and an argument of type `b` and returns a new state
along with a return type of type `a`. If we have a map as our state where we store
data we can create functions like this.

```haskell
    type State = Map String Int -- like a `typedef` in C

    getFromState :: State -> String -> (State, Int)
    getFromState state key = (map, lookupWithDefault 0 key state)

    putToState :: State -> String -> Int -> (State, ()) -- returns `void`
    putToState state key val = (insert key val state, ())
```

now we can write functions that use these functions

```
    main = 
        let (st0, _) = putToState empty "x" 5
            (st1, _) = putToState st0 "y" 2
            (st2, x) = getFromState st1 "x"
            (st3, y) = getFromState st2 "y"
            (st4, _) = putToState st3 "z" (x + y)
            (st5, z) = getFromState st4 "z"
            in do print z
```

This will print '7'. Each one of these functions updates a state and also returns a
state. It is kind of verbose and hard to follow, but luckily we can create a monad
to encapsulate this!

```
    data StateM a = StateM (State -> (State, a))
    instance Monad (StateM s) where
        return x = StateM (\s -> (s, x))
        (>>=) (StateM fn1) fn2 = StateM (\s ->
                        let (newstate, a) = fn1 s
                            (StateM fn) = fn2 a
                            in
                            fn newstate
                        )

```

This probably looks very confusing, but if you work it out it might statrt to make sense.
All this is doing is encapsulating the explicit passing of state around. We can now implement
functions like these.

```haskell
    getFromState :: String -> StateM Int
    getFromState key = StateM $ \state -> (state, getWithDefault 0 key state)

    putToState :: String -> Int -> StateM ()
    putToState key val = StateM $ \state -> (insert key val state, ())

    runState :: StateM a -> a
    runState (StateM fn) = fn empty -- run the state starting with an empty map
```

Now we can use this monad with

```haskell
    main =
        let z = runState $ do
                    putToState "x" 5
                    putToState "y" 2
                    x <- getFromState "x"
                    y <- getFromState "y"
                    putToState "z" (x + y)
                    getFromState "z"
        in print z
```

Now it is starting to look like something a little more familiar from
imperative programming languages. It is almost semantically the same
as the long-winded example, except all the state passing is done
under the covers by the monad.

Haskell has a more general version of this State monad called `State`, but
instead of restricted to using a `Map String String` as its state, it can
use any type and lets the user retrieve and set that state.

#### The IO Monad

As stated way back in the beginning, Haskell is a pure functional language that has
no side effects. Does that mean that Haskell does not allow the user to read a write
from a file or print to the console? NO! Instead, all these operations are wrapped
in what is the most important monad of all, the IO monad. This is a monad that can
be thought to carry around state. We can think of it as like the state monad above,
but rather than carrying around a `Map String String` as its state, it carries around
the _entire_ universe as its state. Of course it isn't implemented this way, but
conceptually that is what it is doing.

When a call to `putStrLn` in called, the IO monad is conceptually rebuilding the universe
except that in the new universe, some bytes have been written to stdout.
