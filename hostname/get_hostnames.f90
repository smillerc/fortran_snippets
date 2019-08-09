module mod_hostname
  use iso_c_binding
  implicit none

  interface
    integer(kind = C_INT) function gethostname(hname, len) bind(C, name = 'gethostname')
      use iso_c_binding
      implicit none
      character(kind = C_CHAR) :: hname(*)
      integer(kind = C_INT), value :: len
    end function gethostname
  end interface
  
  contains
  
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
          fn(i: i) = c
        end do
        do j = i, str_len
          fn(j: j) = ' '
        end do
        host_name = trim(fn)
      else
        host_name = ''
      end if
    end function get_host_name
  
end module mod_hostname

program hostname
  use mod_hostname
  implicit none

  write(*,*) 'Hostname: "' // get_host_name() // '" for image: ', this_image()
end program hostname
  