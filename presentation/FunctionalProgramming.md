Functional Programming
----------------------

In traditional imperative programming we define functions/classes/methods and then starting with main we give the computer a series of step by step instructions that it should perform. However with functional languages you are not telling the computer what to do step by step but instead your telling it what things are. For example in c you can do something like:  

    int main(int argc, char* argv[]){
        int x = 4;
        x+= 3;

        printf("Your integer: %d\n", x);
    }


This tells the computer to set up the variables step by step then print the solution of the addition. However in functionally programming you can’t do this. You told the computer x was 5 then you told it x was something else? We don’t want to be liars because then we would be no better than Darth Vader himself. Instead we have to do something like:  


    main = let x = 4 in
        y = x+3
        printStrLn("Your integer: " ++ y



<!---
At the bottom of every page we need a next and previous button 
-->

<hr>
[Home](../README.md) | [Back](../README.md#table-of-contents) | [Next](http://www.google.com)

