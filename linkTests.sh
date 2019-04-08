#!/bin/bash
rm -rf bin;

mkdir bin;

echo "Linking...";

for filename in mips/*.s; do
	b=$(basename "$filename");
	n="${b%%.*}";
	echo "\t $n";
	gcc runtime.c "$filename" -o "bin/$n" -lgc
done;
echo "Done!"
