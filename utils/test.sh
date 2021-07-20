cargo build;
for file in $(find ./test/ -path '**/*test/*.sy' -name '*.sy'|sort -n)
do
    echo "Compiling $file"
    ./target/debug/sysycompiler $file
done
