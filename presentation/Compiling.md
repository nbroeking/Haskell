Compiling
---------

Of course just writing code is pretty useless unless we have a way to actually run it. With Haskell there is three major ways to run code. You can either run it in an interpreter, using the Haskell interactive shell, or compile it nativity. Both have their advantages and of course because we are two very nice guys we will show you both as well as talk about the advantages and disadvantages of both. 

####Interpreter 
Haskell has a very advanced interpreter, it is called RunHaskell and it can be invoked by calling RunHaskell <file>. The RunHaskell Interpreter may or may not be JIT. It depends I haven’t done enough research on it to be sure yet. 

####Natively Compile
The Glasgow Haskell Compiler or ghc is the main Haskell compiler. It will compile Haskell down to native code and will create a binary that can be run just like any c, or c++ compiled program. Obviously, this has some major benefits because you will get some incredibly fast and efficient programs if they are run natively.  

To invoke the Glasgow Haskell Compiler first create a haskell program.
‘''
<!---
At the bottom of every page we need a next and previous button 
-->
<hr>
[Home](../README.md) | [Back](../README.md#table-of-contents) | [Next](http://www.google.com)

