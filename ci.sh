#!/bin/sh

set -ex

THIS=$PWD
cargo build --release

cd $THIS/tests
./ci.sh

for i in $THIS/examples/*
do
    cd $i
    ../../target/release/zz --smt-timeout=20000 clean
    ../../target/release/zz --smt-timeout=20000 run
    ../../target/release/zz --smt-timeout=20000 test
done

for i in $THIS/modules/*
do
    cd $i
    ../../target/release/zz --smt-timeout=20000 clean
    ../../target/release/zz --smt-timeout=20000 test
done

echo
echo all passed
