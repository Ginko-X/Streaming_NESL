#!/bin/sh

# Compare the works and steps of an SNESL program and its translated SVCODE program
# with different input sizes

# Usage: ./runSNESL <SNESLexe> <SNESLprogname> # no need the extension '.sed'

n=(100 300 500 700 1000) # 3000 5000) # input size ranges
EXE=$1  
FILE=(prime) # comp guardF guardT iota)

PLOT=./plot.py

for ((j=0; j< ${#FILE[@]}; j++)); do	
    FILEC=${FILE[$j]}".cost"
	rm -f ${FILEC}
	echo "For input size: "${n[*]}"\n" >> ${FILEC}
	for ((i=0; i< ${#n[@]} ; i++)); do
	    sed "s/arg?/${n[$i]}/g" ${FILE[$j]}".sed" > ${FILE[$j]}"_${n[$i]}.snesl"    
	    ${EXE} ${FILE[$j]}"_${n[$i]}.snesl" >> ${FILEC}
	    echo "\n" >> ${FILEC}
	done
	rm ${FILE[$j]}"_"*
	${PLOT} ${FILE[$j]}
done


	
