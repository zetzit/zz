#!/bin/sh

set -ex

THIS=$PWD
cargo build --release

cd $THIS/tests
./ci.sh

for i in $THIS/examples/*
do
    cd $i
    ../../target/release/zz clean
    ../../target/release/zz run
    ../../target/release/zz test
done

for i in $THIS/modules/*
do
    cd $i
    ../../target/release/zz clean
    ../../target/release/zz test
done

echo
echo all passed
