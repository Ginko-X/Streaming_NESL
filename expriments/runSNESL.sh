#!/bin/sh

# Compare the works and steps of an SNESL program and its translated SVCODE program
# with different input sizes

# Usage: ./runSNESL <SNESLexe> <SNESLprogname> # no need the extension '.sed'

n=(10 20 30 40 50) # 60 70 80 90 100 200 300) # input size ranges
EXE=$1  
FILE=$2
FILEC=${FILE}".cost"
PLOT=./plot.py


rm -f ${FILEC}
echo "For input size: "${n[*]}"\n" >> ${FILEC}

for ((i=0; i< ${#n[@]} ; i++)); do
    sed "s/arg?/${n[$i]}/g" $FILE".sed" > $FILE"_${n[$i]}.snesl"    
    ${EXE} $FILE"_${n[$i]}.snesl" >> ${FILEC}
    echo "\n" >> ${FILEC}
done

rm $FILE"_"*

${PLOT} ${FILE}

	
