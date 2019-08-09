#!/bin/bash
#PBS -l select=2:ncpus=36:mpiprocs=36
#PBS -q s2
#PBS -N caf_test
#PBS -j oe
#PBS -m a
#PBS -m e

cd /b1/smil/coarray_fortran_sandbox/hostname

module purge
module load intel/2019.3

export I_MPI_REMOVED_VAR_WARNING=0
export I_MPI_VAR_CHECK_SPELLING=0

rm -rf caf_config.txt std.out
echo "-genvall -perhost 2 -genv I_MPI_FABRICS=shm:ofi -n 72 ./a.out" > caf_config.txt

./a.out > std.out

exit
