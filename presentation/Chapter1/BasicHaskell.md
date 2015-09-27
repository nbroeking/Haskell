Basic Haskell
-------------

Now that we have confused you with all these fancy terms lets try to clear the fog. We are going to walk through a very basic Haskell program to jump us in to the language. As most programming languages, the entry point is called main. When you start your program the first thing that gets called is main. 

In C we may have something like

```C
int main(){
	//Do stuff
}
```

In Haskell main returns void or () and does Input/Output. So its return type is IO(). We will talk more about what this means in the Monad section… But we can write this as.

```Haskell
main::IO() —main returns IO()
main = —do stuff
```

Lets quickly explain what is going on. The first line, you can think of like a declaration. Now if you remember everything is a function and so main is a function that returns IO(). The next line we are specifying with main does. main = //function is defining the function. Lets take this a step further. Lets create main that declares two variables, adds them together, and prints the result. Our code looks like

```Haskell
main::IO()
main = let x = 4
           y = 5
           z = x + y
       in 
           print z
``` 

To understand whats going on we need to step this into parts. Haskell is a functional language, we know we have said this at least 100 times but when you wrap your head around what that means it will make a lot more sense. We are defining a function, main and what does main do, it prints z. However in order for you to evaluate the expression you need variables. That is where the let comes in. In english we might say something like let x = y = z in the function print z. That is exactly what main function is doing but without all the filler words. 

####Programming Basics
Now that we know basic Haskell we will take it even further to the basics that every programming language has. Functions, If statements, and loops. In order to show you this section we are going to create a basic factorial program that solves factorial for the number 20. 

#####Functions
Functions in Haskell are really easy they work just like main. You specify a name then what it does. For example our factorial function we can do something like this:

```Haskell
main::IO()
main = let z = factorial 20
       in
       	print z

factorial Integer -> Integer
factorial x = --solve factorial
```

The factorial Integer -> Integer says that the function factorial takes an Integer -> and returns an Integer. The next line says factorial with one parameter x = function. We will expand this out a little more below but its important to understand a few things about Haskell functions first. Haskell supports currying. So if you have factorial that takes two ints and returns a String you will write something like:

```Haskell
function Integer -> Integer -> String
function x y = function
```
You can call the function with two parameters function 2 4 and get a string back but if you only give it one integer, function 1, you wont get a string back but you will get a function that takes an int and returns a string back. Magic I know. 

The last important thing about functions in Haskell is that they are first class. That means that you treat functions no differently than any other data type in Haskell. You can have variables equal functions, pass functions in as parameters, or do pretty much whatever else you want with them.

#####If Statements
If statements in Haskell exist and for the purposes of this tutorial we will show you all about them. However, after we show you the pattern matching magic that Haskell performs for us you will probably never use them again. 

If statements in Haskell have three parts. if then else, 

```Haskell
if x
then x
else y
```
We can extend this to our factorial example. With this we can actually implement the factorial section of our program.

```Haskel
main::IO()
main = let z = factorial 20
       in
       	print z

factorial Integer -> Integer
factorial x = if x == 0
				 then 1
				 else
				 x * factorial (x - 1)
```

#####Pattern Matching 
With Haskell you don't need to do explicit if statements you can do pattern matching. Haskell can know which one you mean by the type of parameter that is passed in. We can re write the above program like:

```Haskell
main::IO()
main = let answer = factorial 20
       in
           print answer


factorial::Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)
```

If the parameter to factorial is 0 it will evaluate to 1 else it will use the other factorial which performs the recursive evaluation.

#####Loops
As far as Haskell is concerned there are no loops. You are not specifying step by step instructions so it is pointless to have loops. However, there is one situation where they are appropriate and that is when dealing with input and output. We go into this more deeply when we talk about monads but we will give you a basic introduction. In Haskell there are commands called do and forever that allow a series of code to be called in a loop. An example of a main run loop in Haskell.

```Haskell
import Control.Monad

main::IO()
main = forever $ do  
    putStr "\nEnter Input:\n"  
    input <- getLine
    
    let x = read input:: Integer
    let answer = factorial x
    print "Answer: "
    print answer

factorial::Integer -> Integer
factorial x = 
            if x == 0
                then 1
            else 
                  x * factorial (x-1) 
```

This program will keep asking the user for input and then calculate the factorial of the input. 