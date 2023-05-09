#!/bin/sh

mkdir -p build
cc -O2 -Wall -Wextra -o build/subcpp src/main.c
