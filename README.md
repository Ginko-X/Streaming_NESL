# Working Progress
+ Week 18 (1~7 May)
    + A new solution to the control change problem, using a new SVCODE instruction "WithCtrl", see [Issue 12](https://github.com/Ginko-X/Streaming_NESL/issues/12)
    + Implemented the sequence expression in a more efficienty way instead of desugaring it to a fold of calling _append function
    
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
