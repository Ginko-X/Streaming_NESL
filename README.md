This is an experimental interpreter for the functional, nested data-parallel language, Streaming NESL, from my [master's thesis](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf):
Previous work about Streaming NESL:
[Streaming nested data parallelism on multicores](http://hiperfit.dk/pdf/p44-madsen.pdf)


`runhaskell Snesl` eager interpreter
or
`runhaskell Snesl -s` streaming interpreter

Interpreter Usage:
   <exp>          Evaluate an expression
   
   <func>         Syntax: function f(x1:type1,...,xn:typen):type = expression

   :l <file>      Load functions from a file

   :d <exp> <file> Generate the DAG file that can be used in graph-easy

   :r <exp> <int> Interpret the `exp` streamingly in a round robin fashion
                     and print out the first `int` rounds of Svctx (unless all
		                       the Procs already shutdown before that round)

   :c <exp>       Display generated SVCODE for the expression

   :fc <func>     Display generated SVCODE for the function

   :bs <int>      Set buffer size

   :m <T/F>       F:SIMD(default),T:MIMD, only works for the streaming interpreter

   :D <exp> <int> run specified line(s) of SVCODE and show the context (only for eager
                     interpreter)

   :q             Exit