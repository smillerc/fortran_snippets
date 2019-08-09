#!/bin/bash

export I_MPI_REMOVED_VAR_WARNING=0
export I_MPI_VAR_CHECK_SPELLING=0

rm -rf a.out
mpiifort -coarray=single hello_world_mix.f90
mpirun -np 3 ./a.out