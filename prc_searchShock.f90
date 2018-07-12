module prc_searchShock
  use mod_szk
  use mod_globals

  implicit none

  integer :: ishkdir
  integer :: itgtaxs, itgt
contains

  subroutine setSearchAxis
    character(4), parameter :: caxs(2) = (/ 'Xi', 'Eta' /)
    do
      write(lwrite,*) 'Which direction do you want? (0: xi / 1: eta)'
      read(lread, *) itgtaxs
      if(itgtaxs==0 .or. itgtaxs==1) exit
    enddo
    write(lwrite,*) 'Target point of ' // trim(caxs(itgtaxs + 1)) // 'is...?'
    read(lread, *) itgt
  endsubroutine

  subroutine setShockDirection
    write(6,*) 'Do you determine shock direction? ( -1/ 1 ) If not, input 0.'
    do
      read(lread,*) ishkdir
      if(ishkdir==0 .or. ishkdir==1 .or. ishkdir==2) exit
    enddo
  endsubroutine

  subroutine searchShock
    integer :: ilen
    integer :: i1, i2, imaxidx
    integer, parameter :: fg_xi = 1 - itgtaxs, fg_eta = itgtaxs
    integer, allocatable :: fg_valid(:)
    real(4), allocatable :: wdat(:), wcod(:)
    real(4) :: wdlt, wmaxdlt

    ilen = fg_eta*lklen + fg_xi*ljlen

    allocate(wdat(ilen+1))
    allocate(wcod(ilen+1))
    allocate(fg_valid(ilen))
    fg_valid = 1
    wcod(1) = sgrd(1, fg_eta*ltgt + fg_xi, fg_eta + fg_xi*ltgt, 1)
    do i1=2, ilen
      wcod(i2) = sgrd(1, fg_eta*ltgt + fg_xi*i2, fg_eta*i2 + fg_xi*ltgt, 1)
      if(wcod(i2) == wcod(i2-1)) fg_valid(i2) = 0
    enddo
    wcod(ilen+1) = wcod(2) + wcod(ilen)
    if(wcod(2) == wcod(ilen)) fg_valid(ilen) = 0
    do i1=1, lscl
      sdat(1, i1) = sfld(ltimidx)
      do i2=1, ilen
        wdat(i2) = sscl(fg_eta*ltgt + fg_xi*i2, fg_eta*i2 + fg_xi*ltgt, 1, lscllst(i1))
        if(wdat(i2) == 0.d0) fg_valid(i2) = 0
      enddo
      wdat(ilen+1) = wdat(2)
      wratmax = 0
      do i2=1, ilen
        if(fg_valid(i2) == 0) continue
        wdlt = wdat(i2+1) - wdat(i2)
        wdlt = (wdlt*ishkdir + (1-abs(ishkdir))*abs(wdlt)) / abs(cod(i2+1) - cod(i2))
        if(wmaxdlt < wdlt)then
          imaxidx = i2
          wmaxdlt = wdlt
        endif
      enddo
      if(imaxdlt == ilen) imaxdlt = 1
      sdat(2, i1) = imaxdlt
      sdat(3, i1) = wcod(imaxdlt)
      sdat(4, i1) = wcod(imaxdlt+1)
      sdat(5, i1) = wdat(imaxdlt)
      sdat(6, i1) = wdat(imaxdlt+1)
    enddo
  endsubroutine

end module
