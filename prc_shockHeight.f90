module prc_shockHeight
  use mod_szk
  use mod_globals

  implicit none

  real(4), allocatable, save, private :: wthh(:), fg_cmp(:), wini(:)

contains

subroutine shockHeightInit
  integer :: i1
  allocate(wthh(lscl))
  allocate(fg_cmp(lscl))
  write(lwrite,*) 'What is threshold of shock? Set each functions:'
  do i1=1, lscl
    write(lwrite,*) bscllst(i1)
  enddo
  read(lread, *) (wthh(i1), i1=1, lscl)
  write(lwrite,*) ' '
  write(lwrite,*) 'What is compare condition?'
  write(lwrite,*) '"data > threshold" is shock: 1.d0'
  write(lwrite,*) '"data < threshold" is shock: -1.d0'
  write(lwrite,*) 'Set each functions:'
  do i1=1, lscl
    write(lwrite,*) bscllst(i1)
  enddo
  read(lread, *) (fg_cmp(i1), i1=1, lscl)
  write(lwrite,*) 'Compare condition is...'
  do i1=1, lscl
    if(fg_cmp(i1) >= 0.d0)then
      write(lwrite,*) bscllst(i1)//' : data > '//toString(wthh(i1)*fg_cmp(i1))
    else
      write(lwrite,*) bscllst(i1)//' : data < '//toString(wthh(i1)*abs(fg_cmp(i1)))
    endif
  enddo
  write(lwrite,*) ' '
  allocate(wini(lscl))
  write(lwrite,*) 'What is initial compare value?'
  write(lwrite,*) 'Set each functions:'
  do i1=1, lscl
    write(lwrite,*) bscllst(i1)
  enddo
  read(lread, *) (wini(i1), i1=1, lscl)
  write(lwrite,*) 'Initial value is...'
  do i1=1, lscl
      write(lwrite,*) bscllst(i1)//' : '//toString(wini(i1))
  enddo
  write(lwrite,*) ' '
endsubroutine

subroutine shockHeightMain
  real(4) :: wmindat, wdat, wnewthh
  integer ::  i, j, k
  integer :: iminidx(2)
  do i=1, lscl
    wnewthh = wthh(i) * fg_cmp(i)
    wmindat = wini(i) * fg_cmp(i)
    iminidx = (/ 1, 1 /)
    do k=lkbgn, lkend
      do j=ljbgn, ljend
        wdat = sscl(j, k, 1, lscllst(i)) * fg_cmp(i)
        if(wnewthh < wdat .and. wdat < wmindat)then
          wmindat = wdat
          iminidx(1) = j
          iminidx(2) = k
        endif
      enddo
    enddo
    sout(1, i) = sfld(ltimidx)
    sout(2, i) = iminidx(1)
    sout(3, i) = iminidx(2)
    sout(4, i) = sgrd(1, iminidx(1), iminidx(2), 1)
    sout(5, i) = sgrd(2, iminidx(1), iminidx(2), 1)
    sout(6, i) = sscl(iminidx(1), iminidx(2), 1, lscllst(i))
    sout(7, i) = wmindat - wini(i)*fg_cmp(i)
  enddo
endsubroutine

end module
