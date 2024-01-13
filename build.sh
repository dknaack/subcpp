#!/bin/sh

mkdir -p build
cc -g3 -Wall -Wextra -Wno-missing-field-initializers -Wno-unused-function -o build/subcpp src/main.c
