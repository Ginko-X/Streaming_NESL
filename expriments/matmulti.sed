-- matrices multiplication
let n = arg?;
    matA = {&n : _ in &n};
    matB = {{x : _ in &n} : x in &n} -- transposition of matA
in {{ reducePlus({x*y : x in a, y in b}) : a in matA} : b in matB}
