Functional Programming
----------------------

Functional Programming is one of the three major paradigms along with
imperative and logic. Regardless of its reputation as being somewhat
esoteric and impractical, functional programming actually has roots
just as deep as imperative programming. 

Traditional imperative programming is what most people are familiar with.
The thread of execution marches through each instruction from top to bottom,
each of which mutate some aspect of state until the program terminates. For
example, we may write a program in C that looks like:

```c
int main(int argc, char** argv)
{
    int list[] = {1, 3, 5, 8, 11, 13};
    int length = sizeof(list) / sizeof(int);
    int i = 0;
    int x;

    for(i = 0; i < length; ++ i) {
        x = list[i];
        x = x * x;
        printf("%d\n", x);
    }
}
```

Functional programming is fundamentally different from imperative programming.
Since functional programming is all about avoiding state, in Haskell, this
program would look fundamentally different.

```Haskell
    main :: IO ()
    main =
        let list = [1, 3, 5, 8, 11, 13]
            loop [] = return ()
            loop (x:xs) = do 
                let xsq = x * x
                print xsq
                loop xs
            in
            loop list
```

Here we construct a list, and also define a recursive function `loop` that
takes the head element, prints the square of the element and calls itself on
the tail until the list is empty.

To exasperate this difference, an avid Haskeller might write

```Haskell
    main :: IO ()
    main = mapM_ (print . (^2)) [1, 3, 5, 8, 11, 13]
```

<!---
At the bottom of every page we need a next and previous button 
-->

<hr>
[Home](../README.md) | [Back](../README.md#table-of-contents) | [Next](http://www.google.com)

