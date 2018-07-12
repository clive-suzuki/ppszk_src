module prc_shockHeight
  use mod_szk
  use mod_globals

  implicit none

  real(4), allocatable :: wthh(:)

contains

subroutine setThreshold
  integer :: i1
  allocate(wthh(lscl))
  write(lwrite,*) 'What is threshold of shock? Set each functions:'
  do i1=1, lscl
    write(lwrite,*) bscllst(i1)
  enddo
  read(lread, *) (wthh(i1), i1=1, lscl)
  write(lwrite,*) 'Threshold is...'
  do i1=1, lscl
    write(lwrite,*) bscllst(i1)//' : '//toString(wthh(i1))
  enddo
endsubroutine

subroutine shockHeight

endsubroutine

end module
