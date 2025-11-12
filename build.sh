#!/bin/sh

mkdir -p build
cc -g3 -Wall -Wextra -Wno-missing-field-initializers -Wno-unused-function -Wno-unused-parameter \
	-Werror=implicit-function-declaration -Werror=return-type -DDEBUG=1 \
	-fsanitize-trap -fsanitize=undefined \
	-o build/subcpp src/main.c
