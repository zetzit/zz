#!/bin/sh
set -ex

THIS=$PWD

for i in $THIS/examples/*
do
    cd $i
    cargo run run
    cargo run test
done

for i in $THIS/modules/*
do
    cd $i
    cargo run test
done
