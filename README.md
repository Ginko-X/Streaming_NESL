+ This is the experimental interpreter from my [master's thesis](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf) work for the functional, nested data-parallel language Streaming NESL (from [Streaming nested data parallelism on multicores](http://hiperfit.dk/pdf/p44-madsen.pdf))


+ `runhaskell Snesl.hs` to use the _eager_ interpreter, or `runhaskell Snesl.hs -s` the _streaming_ interpreter

+ Interpreter Usage (language syntax can be found in my thesis, or have a look at some [examples](https://github.com/Ginko-X/Streaming_NESL/tree/master/examples)):
```
   <exp>          Evaluate an expression
  
   <func>         Define a function; syntax: function <fname>(<x1>:<type1>,...,<xn>:<typen>):<type> = <exp>
   
   :l <file>      Load functions from a file

   :c <exp>       Display generated SVCODE for the expression

   :fc <fname>    Display generated SVCODE for the function

   :bs <int>      Set buffer size
   
   :r <exp> <int> Run the first `int` scheduling rounds and display the context (only for streaming interpreter)

   :D <exp> <int> Run specified line(s) of the generated SVCODE and show the context (only for eager interpreter)
   
   :d <exp> <file> (Disabled) Generate a text file that can be used to visualize the DAG with graph-easy 

   :m <T/F>       (Disabled) Choose SIMD/MIMD model, F:SIMD(default),T:MIMD (only for streaming interpreter)

   :q             Exit
  ```
