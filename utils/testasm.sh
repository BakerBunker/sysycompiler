#!/bin/bash

for file in $(find ./test/ -path '**/*test/*.sy' -name '*.sy'|sort -n)
do
    echo "Testing $file's ASM"
    ./target/debug/sysycompiler $file --asm --test_asm
done