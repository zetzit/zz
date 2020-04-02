#!/bin/sh

set -ex

THIS=$PWD
cargo build --release

cd $THIS/tests
./ci.sh

for i in $THIS/examples/*
do
    echo "================="
    cd $i
    ../../target/release/zz --smt-timeout=200000 clean
    ../../target/release/zz --smt-timeout=200000 run
    ../../target/release/zz --smt-timeout=200000 test
done

for i in $THIS/modules/*
do
    cd $i
    ../../target/release/zz --smt-timeout=200000 clean
    ../../target/release/zz --smt-timeout=200000 test
done

echo
echo all passed
