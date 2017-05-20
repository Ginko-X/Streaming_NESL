# Working Progress
+ Week 20 (15~21 May)
    + Modified the implementation of user-defined functions
        - syntax: `function f(x1:type1,...,xn:typen) : type = exp`
        - examples: see `examples/prime.snesl`
    + Implemented interactive interpretation (using Haskeline)
        - `runhaskell Snesl.hs`
        - Usage:          
            - `<expression>` evaluate an expression (also include type-check, compiling to SVCODE, and comparison of SNESL and SVCODE results)
            - `<function definition>` 
		    - `:load <filename>`   load functions from a file
			- `:quit`

+ Week 19 (8~14 May)
    + Modified/optimized the structure of STree as discussed in [#12](https://github.com/Ginko-X/Streaming_NESL/issues/12). The new STree includes not only the SIds but also the type information (of the value it represents). This helps simplify the SNESL compiler code.
    + Add user-defined functions, **in process** : 
        - support reading an SNESL program from a file             
            - syntax: `def x = exp`, `function f(x1,...,xn) = exp` 
            - usage (after loading `snesl-example.hs`): `runFile <filename>`, e.g. `runFile "examples/sqsum.snesl"`
        - SNESL parser, interpreter, typing **done** 
        - Compiler **in process**
    
+ Week 18 (1~7 May)
    + A new solution to the control change problem, using a new SVCODE instruction "WithCtrl", [see details Issue 12](https://github.com/Ginko-X/Streaming_NESL/issues/12)
    + Implemented the sequence expression in a more cost-efficient way instead of desugaring it to a fold of calling `_append` function, [see details Issue 13](https://github.com/Ginko-X/Streaming_NESL/issues/13)
    
+ Week 17 (24~30 Apr)
    + Provide a solution to the problems discussed in [#4](https://github.com/Ginko-X/Streaming_NESL/issues/4), for [details](https://github.com/Ginko-X/Streaming_NESL/issues/10)
    + Cost model experiment and analysis, see [some results](https://github.com/Ginko-X/Streaming_NESL/issues/11)
    + Some trivial work that may be not interesting
        + Bug fix: getVars, let-bindings translation and so on
        + Haskell (Monad) review, modify some Monads (including SneslTrans, Svcode) and some other code improvement
        + NESL papers review
    
  
+ Week 16 (17~23 Apr)
    + Add sequence expressions ([some discussion](https://github.com/Ginko-X/Streaming_NESL/issues/4))
    + Add work and step cost to SNESL
    + Add work and step cost to SVCODE, [for details](https://github.com/Ginko-X/Streaming_NESL/commit/8aabcf45a7d26202c738c8d246190cb03646ece1)
    + Add the folder 'experiments' (and also 'examples' for SNESL programs) to compare the empirical costs of a SNESL program and its translated SVCODE program, ([see some results](https://github.com/Ginko-X/Streaming_NESL/issues/9))
    + Found and fixed some bugs


+ Week 15 (10~16 Apr, Easter break)
    + [Thesis proposal](https://docs.google.com/document/d/1xeS902Cb_PUidC7BxDZYzvpoMYaEfMEPG5s9OC0j_gg/edit?usp=sharing)(editable)
    + [bug fix](https://github.com/Ginko-X/Streaming_NESL/commit/23a97947f47920117e0840ec92ccc6b9bf8f0c5f) about 'dataTransBack' function
    + adding sequence expressions in SNESL in process
    
+ Week 12-14
    + implemented a minimal SNESL including the following modules:
        + SNESL parser, interpreter, type inference
        + SVCODE (the target language of SNESL) interpreter
        + compiler from SNESL to SVCODE
        + data transformation between SNESL and SVCODE values	
