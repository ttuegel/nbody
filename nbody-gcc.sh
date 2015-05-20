#!/bin/sh

gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse3 nbody-gcc.c -o nbody-gcc -lm
