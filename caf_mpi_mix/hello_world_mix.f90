program hello_mixed

  ! Compile with: mpiifort -g -coarray=single hello_world_mix.f90
  ! Run with: mpirun -np 3 ./a.out

  ! Some discussion at https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/622116
  ! TL;DR - the coarray library under the hood does it's own mpi inits and whatnot
  !       - compile with -coarray=single, b/c the default behavior for multiple caf images is that 
  !       - the 1st image effectively does it's own mpirun for the remaining images

  use mpi
  implicit none
  integer :: ierr, rank, comsize, i

  ! call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)


  print *, 'MPI - Hello, world, from image ', rank, 'of ', comsize, '!'
  print *, 'CAF - Hello, world, from image ', this_image(), 'of ', num_images(), '!'

  ! Since this is compiled with -coarray=single, no need for MPI_Init or _Finalize
  ! call MPI_Finalize(ierr)
  
  sync all

  do i = 1, num_images()
    if ( this_image() == i) then
      print*, 'For CAF image ', i, ': the mpi rank is:', rank
    end if
  end do

end program