import SneslInterp

-- An example program: split a string into words
prog1 = 
    "let x = \" streaming   nesl \"    in " ++  -- a string argument
    "let len = #tab(x)       in " ++   -- length 
    "let idx = &len          in " ++   -- indices 

    -- remove the spaces in the string
    "let xfilter = {c: c in x| not(ord(c)==32)} in let vx = tab(x)  in "++

    -- get the indices of spaces -- never used 
    "let spaceidx = {i: i in idx | ord(vx[i])== 32} in "++ 

    -- set flag sequence for partition
    "let y = {let b = ord(c)==32 in the({True | b} ++ {False | not(b)}) : c in x } in "++ 

    -- add a "True" flag as the end
    "let y2 = y ++ {True|True} in "++  

    -- partition into words
    "let res = part(xfilter,y2) in "++

    -- remove spaces
    "{seg: seg in res | not(#tab(seg)==0)} "


main = doExp prog1