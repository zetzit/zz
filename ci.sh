#!/bin/sh

set -ex

THIS=$PWD
cargo build --release

cd $THIS/tests
./ci.sh

for i in $THIS/examples/*
do
    cd $i
    ../../target/release/zz --smt-timeout=200000 clean
    ../../target/release/zz --smt-timeout=200000 run
    ../../target/release/zz --smt-timeout=200000 test
done

for i in $THIS/modules/*
do
    
    if  [ ! "$OSTYPE" = "msys" ] || [[ ! "$i" = *"/io" && ! "$i" = *"/net" ]]; then
        cd $i        
        ../../target/release/zz --smt-timeout=200000 clean
        ../../target/release/zz --smt-timeout=200000 test
    else
        echo "skip module $i"
    fi
done

echo
echo all passed
