#!/bin/bash

INPUT_FILE="./main.don"
BUILD_DIR="./build/donout"
COMPILE_FLAGS=""
COMPILE_FLAGS="-p"

# make sure the build directory exists
mkdir -p $BUILD_DIR

./build/donlang $INPUT_FILE -o $BUILD_DIR/main.ll $COMPILE_FLAGS $1 \
&& clang $BUILD_DIR/main.ll -Wall -lm -o $BUILD_DIR/main

if [ $? -eq 0 ]; then
		./$BUILD_DIR/main
		
		echo "Program exited with code $?"
fi
