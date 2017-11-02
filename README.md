# Working Progress
+ 2 Nov: [presentation slides](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/presentation/main.pdf)
+ 1 Nov: fixed work preservation proof
    + for the high-level work cost of general comprehension, I think it should be `(l+1)(k+1) + sum...`, (not `1 + (l+1)k + sum...`, or `1 + (k+1)l) + sum...`,)  where (l+1) accounts for the length of the descriptor, (k+1) for Distr (of k free variables) and _Usum_; the work cost can be bounded now with C >=2 in both cases.
    + reorganized/rewrote/simplied the proof case of general comprehension (of the translation correctness theorem) to make it more readable 

+ Week 51 (17~ 24 Oct)
    + fixed and completed chapter 1
    + fixed chapter 2
    + unfixed/unfinished parts:
        + chapter 2.6.1, 2.6.2, 2.6.7
        + chapter 3 fixed (with new well-formed rules) but maybe still buggy
        + scaling up
        + conclusion
        + bibliography
     + 24 Oct: finished all the parts, fixed known bugs
     
+ Week 50 (10 ~ 16 Oct)
    + Fixes and complements to chapter 1 and 2,  [main.pdf](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf)  
    + proof fixed in chapter 3, except for the last main theorem proof (but should mainly be symbol updates or clean-ups)
    + several small subsections and two lemma proofs are still missing
    
+ Week 49 (3 Oct ~ 9 Oct)
    + [reorganization of thesis](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf); sketched out chapter 1 and 2 (some parts still missing or buggy), chapter 3 not updated yet 
    
+ Week 48 (26 Sep ~ 2 Oct)
    + mainly worked on thesis Chapeter 2: Implementation [(Page 4~13)](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf) (for other parts just adjusted the thesis structure, almost no update) 
    
+ Week 47 (18 ~ 25 Sep)
    + Formalization:
        + fixed WithCtrl structure and added proof of concatenation lemma
        + updated to SNESL Level-1: add using variables for SNESL comprehension exps (related changes of grammar, judgment rules and correctness proof)
	
+ Week 46 (11 ~ 17 Sep)
    + Code: changed SFun and WithCtrl structure: removed their dependency on STree
    + Formalization: version 0.0.7
        + main correctness theorem proof (section 3.3): fixed some bugs discussed in last meeting
        + before Section 3.3
            + added WithCtrl import and export lists, changed related evaluation/translation rules
            + got stuck in/some question about concatenation lemma proof[#26](https://github.com/Ginko-X/Streaming_NESL/issues/26)
	
    
+ Week 45 (4 ~ 10 Sep)
    + SNESL formalization [(level-0 version 0.0.5)](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf)
        + (almost) finished the proof of main correctness theorem, but still some small problems (marked in pdf)
+ Week 44 (28 Aug ~ 3 Sep)
    + rewrite Xducers with `loopu`/`loopuv` 
        + fixed `Pack` within a WithCtrl
        + deleted two instructions `MapConst` and `SegMerge`
    + [SNESL formalization Level-0 (draft 0.0.2)](https://github.com/Ginko-X/Streaming_NESL/blob/master/Report/main.pdf)
        + proof not finished (got stuck in the comprehension proof case)


+ Week 43 (21~27 Aug)
    + Toy verison correctness proof bug fixes
    + Rewrote Xducers with `loop0`, fixed all known bugs, but code still messy
    
+ Week 42 (14~20 Aug)
    + Started correctness proofs with a toy version exercise [#25](https://github.com/Ginko-X/Streaming_NESL/issues/25)
    
+ Week 41 (7~13 Aug)
    + Streaming interpreter [`SvcodeSXInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeSXInterp.hs) code optmization and bug fixes
        + replaced list of pair with Data.Map for the structure of `Svctx`
        + added `REnv` in `SvcodeP` Monad for RSId mappings used when unfolding SCalls
            + buggy, work in process
    + Cost model bug fixes
    + Related discussions [#20](https://github.com/Ginko-X/Streaming_NESL/issues/20)
    
+ Week 40 (31 Jul ~ 6 Aug)
    + bug fixes for recursion
    + bug fixes for the cost model of the streaming interpreter
    + for details see [#20](https://github.com/Ginko-X/Streaming_NESL/issues/20)

+ Week 39 (24 Jul ~ 30 Jul)
    + Add recursions to the streaming interpreter [`SvcodeProcInterpLong.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeProcInterpLong.hs)
        + modified to unfold functions during runtime
        + fixes of `WithCtrl` interpretation to support recursions, still buggy
	
+ Week 29 (17 Jul ~ 23 Jul)
    + Modified the scheduling strategy from _looping scheduling_ (mentioned in [#19](https://github.com/Ginko-X/Streaming_NESL/issues/19)) to some "fully-filled" looping scheduling, to maintain the correctness of cost model        
        + a buffer is not allowed to be read by its clients before it is full, unless it is the last chunk of the stream
        + when deadlock happens, stealing will be taken firstly to try to break it
        + will be modified further to the two-phase scheduling discussed in the 2nd comment of [#19](https://github.com/Ginko-X/Streaming_NESL/issues/19)
        + [some notes at the 8th comment of #18](https://github.com/Ginko-X/Streaming_NESL/issues/18)
    + implemented the two-phase scheduling mentioned in the 3rd small bullet above, a new module [`SvcodeSInterp.hs`](https://github.com/Ginko-X/Streaming_NESL/blob/master/SvcodeSInterp.hs)
        + done, [some notes at the 5th comment of #19](https://github.com/Ginko-X/Streaming_NESL/issues/19)
        + added command `:m <T/F>` to specify MIMD/SIMD(default) model in interactive interpreter
    + Bugfix of the cost model
        + done, but slightly different from the requirements of #19, [explanation at 5th comment of #19](https://github.com/Ginko-X/Streaming_NESL/issues/19)

    
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
