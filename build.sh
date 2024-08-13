#!/bin/sh

mkdir -p build
cc -g3 -Wall -Wextra -Wno-missing-field-initializers -Wno-unused-function \
	-Werror=implicit-function-declaration -Werror=return-type -DDEBUG=1 \
	-o build/subcpp src/main.c
