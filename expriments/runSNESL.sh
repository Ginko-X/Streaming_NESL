n=(1 10 100 1000 10000)
EXE=../snesl-example
FILE=sqsum
FILEC=${FILE}"_cost.txt"

rm -f ${FILEC}
echo "For input size: "${n[*]}"\n" >> ${FILEC}

for ((i=0; i< ${#n[@]} ; i++)); do
    sed "s/arg?/${n[$i]}/g" $FILE".sed" > $FILE"_${n[$i]}.snesl"    
    ${EXE} $FILE"_${n[$i]}.snesl" >> ${FILEC}
    echo "\n" >> ${FILEC}
done
	
