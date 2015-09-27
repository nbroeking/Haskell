About Haskell
-------------

Haskell is a very strong, statically typed, pure functional programming
language featuring lazy evaluation and a polymorphic type system.

That is a lot of Greek thrown out there, so we'll take each of them
on one at a time.

#### Haskell is Statically Typed

Haskell, like Java, C, and C++, is statically typed, so the type of each
variable is known at compile time; however, unlike most other languages,
Haskell's type system is so strong that it will not implicitly convert anything.

Consider this C program:

```c
    void print_float(float f)
    {
        print("%f\n", f);
    }

    int main(int argc, char** argv)
    {
        int x = 3;
        print_float(x);
    }
```

This will trivially print '3.0', however, this program in Haskell would look something
like this:

```haskell
    printFloat :: Float -> IO ()
    printFloat f = print f

    main :: IO ()
    main =
        let x = (3::Int) in
            printFloat (realToFrac x)
```

Notice that we need to explicitly convert everything.

#### Haskell is Pure

This is what throws most beginners off of Haskell; Haskell is pure. That means
that Haskell avoids side-effects. In fact, Haskell is so strict that it doesn't
even allow them! However, to get around this restrictions, the developers of
Haskell invented the IO monad to *represent* the state of the universe for all
impure functions.

#### Haskell is Lazily Evaluated

One of the coolest features of Haskell, as well as one of the hardest to
control is Haskell's Lazy Evaluation.

In Haskell, no value is computed until it is absolutely needed. This leads to very
cool features of the language as well as adding expressiveness to the language.

```haskell
    main :: IO ()
    main =
        let evens = [0, 2 ..] in
        print (take 500 evens) -- print the first 500 even numbers
```

so unlike other languges like C or Java, we can define a list and have it not be
evaluated until we actually need it.

Lazy evaluation is important for more reasons than just some esoteric linked lists; it
allows the programmer to write expressively while maintaining performance. Consider
the program.

```haskell
    main :: IO ()
    main = do
        fileContents <- readFile "/usr/share/dict/cracklib-small"
        fileLines <- lines fileContents
        print (take 500 fileLines)
```

This program prints just 500 lines from a file that has thousands. While it looks like the program
reads in the whole file at once, it actually does not! It reads the file in chunks as needed.
