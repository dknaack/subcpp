#!/bin/sh

mkdir -p build
cc -O2 -Wall -Wextra -Wno-unused-function -o build/subcpp src/main.c
