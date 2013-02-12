#!/usr/bin/env bash

# $1 :: path
#        Python src file name
# $2 :: Int
#        Start point of the pipeline:  
#           0 stands for "lexer",
#           1 stands for "parser"
#           2 stands for "translator"
#           3 stands for "desugar"
#           4 stands for "anormal"
#           5 stands for "analyze"
# $3 ::  End pointof the pipleine
#         

#echo "Hi there"$0
#echo $0
echo $1
#echo $$1


PROGARRAY=('./pylex2' './pyparse' './pytrans2' './pydesugar' './anormalpy-nt' './pycesk-summarize')
SIZE=${#PROGARRAY[@]}

SUFIX=('.lexed' '.parsed' '.trans' '.desug' '.anormalnt' '.info')

INPUT=$1
#echo $INPUT
OUTPUT=$1
let IDX_I=0

for (( i=$2; i<=$3; i++)); do
   # echo ${PROGARRAY[${i}]}
   if [ $i -ne 0 ] ; then
       let IDX_I=i-1
#       echo $1${SUFIX[$IDX_I]}
       INPUT=$1${SUFIX[$IDX_I]}   
       
   else
       INPUT=$1
       #echo $INPUT
   fi
   OUTPUT=$1${SUFIX[${i}]}
   #echo $OUTPUT

   if [ -e $OUTPUT ] ; then
       echo $OUTPUT
   else
       `touch $OUTPUT`
   fi
   echo "To execute: "
  
   `${PROGARRAY[${i}]} < $INPUT >$OUTPUT`
   
done


 

