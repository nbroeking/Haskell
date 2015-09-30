Functional Programming
----------------------

Functional Programming is one of the three major paradigms along with imperative and logic programming. Functional programing has held onto an unfair reputation as being somewhat esoteric and impractical, however not only is it extremely powerful but we will show you how wrong this reputation is.

You are probably familiar with a different paradigm, imperative programming, where the thread of execution marches through each instruction from top to bottom. C, c++, java, and most traditional languages are all apart of the imperative paradigms. Learning the functional paradigm not only improves your coding ability in languages like Haskell, but will greatly help your skills in imperative languages.

In order to show you the power of functional programming we will start with a quick example. In c we can write the following function.

```c
int main(int argc, char** argv)
{
    int x = 0;
    int y = 3;
    x = y + x;
    
    printf("%d", x);
}
```
In this code we are specifying each instruction for the machine to do in order. The program will execute each instruction one after the other until it reaches the end of the function. However, functional programming is fundamentally different from imperative programming. We aren't trying to tell the computer what to do step by step but rather tell the computer what everything is.

This is an important fundamental concept that defines functional programming. In Haskell if we try to do the same thing as above the code will fail.

```Haskell
    main :: IO ()
    main =
        let x = 0
        	  y = 3
        	  x = y + x     This line won't compile
        in
            print x
```

We already told the program that x was equal to 0. How could we then tell it that x is something different a line later. This would make us liars if we told the program that x was two different things. This means that you can't change the value of a variable. All variables in a functional language are immutable, once you set their values you can not go in and change them. 

You think of imperative programing as having a global state and then each statement will change this global state. For functional programming there is no changeable state. You create a state and if you need to change something you create a new state and overlay it on top of the old one.

The other important feature that sets functional languages apart from imperative ones is that functional languages have first class functions. This means that functions are data. There is no difference between creating a int and a function and we define them the same way.
 
If this didn't make much sense don't worry your probably not alone. Click the next button and we will walk you through some basic Haskell to bring you up to speed.

<!---
At the bottom of every page we need a next and previous button 
-->

<hr>
[Home](../../README.md) | [Back](../../README.md#table-of-contents) | [Next](AboutHaskell.md)