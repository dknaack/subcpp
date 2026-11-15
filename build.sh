#!/bin/sh

cflags="-g3 -Wall -Wno-unused -fsanitize-trap -fsanitize=undefined"

mkdir -p build
cc $cflags -o build/subcpp src/main.c
cc $cflags -o build/test src/test.c
