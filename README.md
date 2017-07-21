# Working Progress
+ Week 29 (17 Jul ~ 23 Jul)
    + Modified the scheduling strategy from _looping scheduling_ (mentioned in [#19](https://github.com/Ginko-X/Streaming_NESL/issues/19)) to some "fully-filled" looping scheduling, to maintain the correctness of cost model
        + a buffer is not allowed to be read by its clients before it is full, unless it is the last chunk of the stream
        + when deadlock happens, stealing will be applied firstly to try to unlock it
        + will be modified further to the two-phase scheduling discussed in [#19](https://github.com/Ginko-X/Streaming_NESL/issues/19) 
    
+ Week 28 (10 Jul ~ 16 Jul)
    + Generalized streaming interpreter to support arbitrary buffer sizes [`SvcodeProcInterpLong.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeProcInterpLong.hs)
        + already incorporated into the interactive interpreter `runhaskell Snesl.hs -s`; default buffersize (i.e., the maximum number of `AVal` a buffer can hold): 10; set buffersize: `:bs <Int>` 
    + Added work/step to the streaming interpreter
    

+ Week 27 (3 Jul ~ 9 Jul)
    + Streaming interpreter `SvcodeProcInterp` bug fixes, optimisation and cleanup, mainly related to Issue [#16](https://github.com/Ginko-X/Streaming_NESL/issues/16) and [#17](https://github.com/Ginko-X/Streaming_NESL/issues/17)
        + documentation see [#21](https://github.com/Ginko-X/Streaming_NESL/issues/21)

+ Week 26 (26 Jun ~ 2 Jul)
    + A basic working streaming interpreter [`SvcodeProcInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeProcInterp.hs):
        + interactive mode: `runhaskell Snesl.hs -s`, also added a new command `:d <exp> <filename>` to generate the DAG file for visualization with `graph-easy`
        + cost not added, but has completed WithCtrl that may work
        + supports user-defined (non-recursive) functions, but still buggy
        + can be deadlocked even in a simple case, such as `let x = 3 in x + x`
	
    
+ Week 25 (19~25 Jun)
    + implemented an SVCODE streaming interpreter using Proc, with many bugs unfixed. Code [`SvcodeProcInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeProcInterp.hs) and [some discussion](https://github.com/Ginko-X/Streaming_NESL/issues/15).
    
+ Week 24 (12~18 Jun) Break, paper review: _Cheap (But Functional) Threads_

+ Week 23 (5~11 Jun)
    + implemented SVCODE instruction transducers and an eager interpreter using these transducers
        + transducer definitions: [`SvcodeProc.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeProc.hs)
        + fully-eager transducer interpreter: add function `sExpInterpProc :: SExp -> Svcode SvVal` in [`SvcodeInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeInterp.hs)
	

+ Week 22 (29 May ~ 4 Jun)
    + reconstruct SVCODE streaming interpreter
        - a starting point [`SvcodeSInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeSInterp.hs)
    
+ Week 21 (22~28 May)
    + Many small modifications to make the language and code clean and clear, include:
        - added if-else-then expressions
        - added two built-in functions: empty, part
        - modified `SFun SId [STree] STree [SInstr]` to `SFun [SId] STree [SInstr]`
        - modified `SCall FId [(SId,SId)] STree` to `SCall FId [SId] [SId]`
        - made `WithCtrl` and `SCall` independent SVCODE instructions
        - removed the `FEnv` component from the compiler monad
        - added `SEnv` into the evaluation monad
    + SVCODE Streaming interpreter start:
        - implemented a basic streaming interpreting model [`SvcodeSInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeSInterp.hs)
            - only support several instrutions at this point (and have bugs in `ToFlag`)
            - specifying streaming compiler/interpreter: `runhaskell Snesl.hs -s`
        
     
+ Week 20 (15~21 May)
    + Modified the implementation of user-defined functions
        - syntax: `function f(x1:type1,...,xn:typen) : type = exp`
    + Implemented interactive interpretation (using Haskeline)
        - `runhaskell Snesl.hs`
        - Usage:          
            - `<expression>` evaluate an expression (also include type-check, compiling to SVCODE, and comparison of SNESL and SVCODE results)
            - `<function definition>`
            - `:l <filename>`   load functions from a file
            - `:q`    quit
    + Implemented recursion 
        - examples: see [`examples/uc_scan.snesl`](https://github.com/Ginko-X/Streaming_NESL/blob/master/examples/uc_scan.snesl)
    + [See some explainations/notes about function(and recursion) implementation](https://github.com/Ginko-X/Streaming_NESL/issues/14)
    
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
