#!/bin/bash

INPUT_FILE="./main.don"
BUILD_DIR="./build/donout"

# make sure the build directory exists
mkdir -p $BUILD_DIR

./build/donlang $INPUT_FILE -p -o $BUILD_DIR/main.ll \
&& clang $BUILD_DIR/main.ll -Wall -lm -o $BUILD_DIR/main

if [ $? -eq 0 ]; then
		./$BUILD_DIR/main
		
		echo "Program exited with code $?"
fi
