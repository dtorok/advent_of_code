#!/bin/bash -e

echo "Compiling..."
ghc Day$1Test.hs -o result

echo "Testing..."
nice -n 15 ./result

echo "Cleanup..."
rm *.hi
rm *.o
rm result
