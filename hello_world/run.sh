#!/bin/bash


rm -rf a.out

ifort -coarray hello_world.f90

export I_MPI_REMOVED_VAR_WARNING=0
export I_MPI_VAR_CHECK_SPELLING=0
export FOR_COARRAY_NUM_IMAGES=37

./a.out