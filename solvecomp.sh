#!/bin/zsh
echo "z3 , yices2, fn"
for i in *.smt2
do


TIME_Z3=$(
    (time z3 $i  >/dev/null)  2>&1  | rev | cut -d ' ' -f 2 | rev | tr -d '.'
)
TIME_YI=$(
    (time yices_smt2  --incremental $i  >/dev/null)  2>&1  | rev | cut -d ' ' -f 2 | rev | tr -d '.'
)

    if [ $TIME_Z3 -gt $TIME_YI ]
    then
        echo -e "\e[31m$TIME_Z3\e[39m , \e[32m$TIME_YI\e[39m , $i"
    else
        echo -e "\e[32m$TIME_Z3\e[39m , \e[31m$TIME_YI\e[39m , $i"
    fi
done
