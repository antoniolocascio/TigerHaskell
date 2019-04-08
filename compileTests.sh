#!/bin/bash
rm mips/*;

echo "Building...";

stack build --silent;

echo "Compiling tests...";

for filename in test/test_code/good/*.tig; do
	echo "\t $filename";
	stack exec -- HaskTiger "$filename" -s -m 
done;
echo "Done!"
