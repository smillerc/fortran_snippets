module sys_info_mod
  use iso_c_binding
  implicit none

  private
  public :: sys_info_t

  interface
    integer(kind=C_INT) function gethostname(hname, len) bind(C, name='gethostname')
      use iso_c_binding
      implicit none
      character(kind=C_CHAR) :: hname(*)
      integer(kind=C_INT), value :: len
    end function gethostname
  end interface

  type :: sys_info_t

    integer :: n_physical_cores = 1              !! Total physicals cores on chip
    integer :: n_physical_cores_per_socket = 1   !! Physical cores per socket
    integer :: n_cpus = 1                        !! Total cpus (including hyperthreading)
    integer :: n_sockets = 1                     !! Total sockets on the motherboard
    integer :: n_threads = 1                     !! Total thread count
    integer :: threads_per_cpu = 1               !! Threads per cpu
    logical :: is_hyperthreaded = .false.        !! Is hypterthreading on?
    character(:), allocatable :: system_type     !! Linux, Darwin, etc.?
    character(:), allocatable :: hostname        !! Full host name

  contains
    procedure, public :: init
    procedure, nopass, private :: get_commandline_result_integer
    procedure, nopass, private :: get_commandline_result_character
    procedure, nopass, private :: run_cmd
    procedure, nopass, private :: get_host_name
  end type

contains

  subroutine init(self)
    class(sys_info_t), intent(inout) :: self
    character(100) :: buffer

    call get_commandline_result_character(str='uname', result=self%system_type)

    if (self%system_type == 'Darwin') then
      call get_commandline_result_integer(str='sysctl -a | grep machdep.cpu.core_count | cut -d":" -f2', &
                                          int_value=self%n_physical_cores)
      call get_commandline_result_integer(str='sysctl -a | grep machdep.cpu.thread_count | cut -d":" -f2', &
                                          int_value=self%n_threads)

      self%n_cpus = self%n_threads
      self%n_physical_cores_per_socket = self%n_physical_cores/self%n_sockets
      self%threads_per_cpu = self%n_threads/self%n_physical_cores

    elseif (self%system_type == 'Linux') then
      call get_commandline_result_integer(str='lscpu | grep "^CPU(s):" | cut -d":" -f2', &
                                          int_value=self%n_cpus)
      call get_commandline_result_integer(str='lscpu | grep "^Socket(s):" | cut -d":" -f2', &
                                          int_value=self%n_sockets)
      call get_commandline_result_integer(str='lscpu | grep "Thread(s) per core:" | cut -d":" -f2', &
                                          int_value=self%threads_per_cpu)

      self%n_physical_cores = self%n_cpus/self%threads_per_cpu
      self%n_physical_cores_per_socket = self%n_physical_cores/self%n_sockets
      self%n_threads = self%n_physical_cores*self%threads_per_cpu
    end if

    if (self%n_cpus > self%n_physical_cores) self%is_hyperthreaded = .true.

    buffer = get_host_name()
    self%hostname = trim(buffer)

  end subroutine init

  subroutine run_cmd(cmd)
    !! Execute a command line call with error checking
    character(*), intent(in) :: cmd
    integer :: cstat, estat
    character(100) :: cmsg

    call execute_command_line(cmd, exitstat=estat, cmdstat=cstat, cmdmsg=cmsg)
    if (cstat > 0) then
      print *, "`"//cmd//"` execution failed with error ", trim(cmsg)
    else if (cstat < 0) then
      print *, "`"//cmd//"` execution not supported"
      ! else
      !   print *, "`"// cmd // "` completed with status ", estat
    end if
  end subroutine

  subroutine get_commandline_result_integer(str, int_value)
    !! Save the output from a command line call and read the integer output
    integer :: f, ierr
    character(:), allocatable :: cmd
    character(100) :: fname
    character(*), intent(in) :: str
    integer, intent(out) :: int_value

    fname = '_tmp_'

    int_value = -1
    cmd = str//' > '//trim(fname)
    call run_cmd(cmd)
    open (newunit=f, file=trim(fname), iostat=ierr)
    if (ierr /= 0) then
      print *, 'Error: unable to open: "'//trim(fname)//'"'
      return
    end if

    read (f, *) int_value
    close (f)

    call run_cmd("rm -rf "//trim(fname))

  end subroutine get_commandline_result_integer

  subroutine get_commandline_result_character(str, result)
    ! Save the output from a command line call and read the character output
    integer :: f, ierr
    character(:), allocatable :: cmd
    character(*), intent(in) :: str
    character(:), allocatable, intent(out) :: result
    character(200) :: buffer
    character(100) :: fname

    buffer = ''
    fname = '_tmp_'

    cmd = str//' > '//trim(fname)
    call run_cmd(cmd)
    open (newunit=f, file=trim(fname), iostat=ierr)

    if (ierr /= 0) then
      print *, 'Error: unable to open: "'//trim(fname)//'"'
      return
    end if

    read (f, *) buffer
    close (f)
    call run_cmd("rm -rf _tmp_")

    result = trim(buffer)

  end subroutine get_commandline_result_character

  function get_host_name() result(host_name)
    character(:), allocatable :: host_name

    integer(kind=C_INT), parameter :: str_len = 100
    character(kind=C_CHAR) :: c_host_name(str_len)
    character(len=str_len) :: fn
    character :: c
    integer :: res, i, j

    res = gethostname(c_host_name, str_len)
    if (res == 0) then
      do i = 1, str_len
        c = c_host_name(i)
        if (c == char(0)) exit
        fn(i:i) = c
      end do
      do j = i, str_len
        fn(j:j) = ' '
      end do
      host_name = trim(fn)
    else
      print *, "call to gethostname() didn't work..."
      host_name = ''
    end if
  end function get_host_name

end module sys_info_mod

program cpu_info
  use sys_info_mod, only: sys_info_t
  implicit none

  class(sys_info_t), allocatable :: sys

  allocate (sys_info_t :: sys)
  call sys%init()

  print *, "# CPUs:", sys%n_cpus
  print *, "# Physical cores:", sys%n_physical_cores
  print *, "# Physical cores / socket:", sys%n_physical_cores_per_socket
  print *, "# Sockets:", sys%n_sockets
  print *, "# Total Threads:", sys%n_threads
  print *, "# Threads / CPU:", sys%threads_per_cpu
  print *, "Is hyperthreaded:", sys%is_hyperthreaded
  print *, "Hostname: ", sys%hostname
  print *, "System Type: ", sys%system_type

end program cpu_info
