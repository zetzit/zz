#!/bin/sh
set -e

THIS=$PWD
cargo build --release



for i in $THIS/mustpass/*
do
    cd $i
    if ../../../target/release/zz run;  then
        echo "$i" passed
    else
        echo "$i" failed
        exit 1
    fi
done


for i in $THIS/mustfail/*
do
    cd $i
    if ../../../target/release/zz check;  then
        echo "$i" passed, but it should not
        exit 1
    else
        echo "$i" "failed to build, as it's supposed to"
    fi
done



echo
echo all passed
