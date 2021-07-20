#!/bin/bash

for file in $(find ./test/ -path '**/*test/*.sy' -name '*.sy'|sort -n)
do
    echo "Testing $file's IR"
    ./target/debug/sysycompiler $file --ir --test_ir
done