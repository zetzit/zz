#!/bin/sh
set -ex

THIS=$(dirname $(readlink -f $0))

for i in $THIS/examples/*
do
    cd $i
    cargo run run
done
