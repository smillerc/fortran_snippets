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

    integer :: ncpus
    integer :: nthreads
    logical :: is_hyperthreaded
    character(:), allocatable :: system_type
    character(:), allocatable :: hostname

  contains
    procedure, public :: init
    procedure, nopass, private :: get_mac_cpu_info_integer
    procedure, nopass, private :: get_host_name
  end type

contains

  subroutine init(self)
    class(sys_info_t), intent(inout) :: self
    character(100) :: buffer


    'lscpu | grep CPU\(s\): | cut -d":" -f2'

    'sysctl -a | grep machdep.cpu.core_count | cut -d":" -f2 > _tmp_'
    'sysctl -a | grep machdep.cpu.thread_count | cut -d":" -f2 > _tmp_'

    if (self%system_type == 'Darwin') then
      call get_mac_cpu_info_integer(search_str='machdep.cpu.core_count', int_value=self%ncpus)
      call get_mac_cpu_info_integer(search_str='machdep.cpu.thread_count', int_value=self%nthreads)
    elseif (self%system_type == 'unix') then
    end if

    if (self%nthreads > self%ncpus) self%is_hyperthreaded = .true.

    buffer = get_host_name()
    self%hostname = trim(buffer)

  end subroutine init

  subroutine get_commandline_result_integer(str, int_value)
    integer :: f
    character(:), allocatable :: cmd
    character(*), intent(in) :: str
    integer, intent(out) :: int_value

    cmd = str // ' > _tmp_'
    call execute_command_line(command=cmd)
    open (newunit=f, file="_tmp_")
    read (f, *) int_value
    close (f)
    call execute_command_line("rm -rf _tmp_")

  end subroutine get_commandline_result_integer

  subroutine get_commandline_result_character(str, result)
    integer :: f
    character(:), allocatable :: cmd
    character(*), intent(in) :: str
    character(:), allocatable, intent(out) :: result

    cmd = str // ' > _tmp_'
    call execute_command_line(command=cmd)
    open (newunit=f, file="_tmp_")
    read (f, *) result
    close (f)
    call execute_command_line("rm -rf _tmp_")
  end subroutine get_commandline_result_character

  subroutine get_mac_cpu_info_integer(search_str, int_value)
    integer :: f
    character(:), allocatable :: cmd
    character(*), intent(in) :: search_str
    integer, intent(out) :: int_value

    cmd = 'sysctl -a | grep '//trim(search_str)//' | cut -d":" -f2 > _tmp_'
    call execute_command_line(command=cmd)
    open (newunit=f, file="_tmp_")
    read (f, *) int_value
    close (f)
    call execute_command_line("rm -rf _tmp_")

  end subroutine get_mac_cpu_info_integer

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

  print *, sys%ncpus, sys%nthreads, sys%is_hyperthreaded, sys%hostname

end program cpu_info
