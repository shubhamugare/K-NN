MAKEFILE_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
DEF=knn
EXT=knn
KOMPILE_BACKEND?=llvm
TESTDIR?=tests
RESULTDIR=tests

%/cycle.kool: kompile
		true
%/threads.kool: kompile
		true

include ./find-k.mak
include ${K_HOME}/include/kframework/ktest.mak
