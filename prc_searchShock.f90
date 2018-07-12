module prc_searchShock
  use mod_szk
  use mod_globals

  implicit none

  integer :: ishkdir
  integer :: itgtaxs, itgt
  character(4), parameter :: baxs(2) = (/ 'Xi', 'Eta' /)
contains

  subroutine setSearchAxis
    do
      write(lwrite,*) 'Which direction do you want? (0: xi / 1: eta)'
      read(lread, *) itgtaxs
      if(itgtaxs==0 .or. itgtaxs==1) exit
    enddo
    write(lwrite,*) 'Target point of ' // trim(baxs(2-itgtaxs)) // ' is...?'
    read(lread, *) itgt
    select case (itgtaxs)
      case(0)
        write(lwrite, *) ' Xi = variable'
        write(lwrite, *) 'Eta = '//toString(itgt)
      case(1)
        write(lwrite, *) ' Xi = '//toString(itgt)
        write(lwrite, *) 'Eta = variable'
    end select
    write(lwrite, *) ' '
  endsubroutine

  subroutine setShockDirection
    write(lwrite,*) 'Do you determine shock direction? ( -1/ 1 ) If not, input 0.'
    do
      read(lread,*) ishkdir
      if(ishkdir==0 .or. ishkdir==-1 .or. ishkdir==1) exit
    enddo
    select case (ishkdir)
      case(-1)
        write(lwrite, *) 'Shock direction is - ' // baxs(itgtaxs+1) // '.'
      case(0)
        write(lwrite, *) 'Shock direction is +- ' // baxs(itgtaxs+1) // '.'
      case(1)
        write(lwrite, *) 'Shock direction is + ' // baxs(itgtaxs+1) // '.'
    end select
  endsubroutine

  subroutine searchShock
    integer :: ilen
    integer :: i1, i2, imaxdltidx
    integer :: fg_xi, fg_eta
    integer, allocatable :: fg_valid(:)
    real(4), allocatable :: wdat(:), wcod(:)
    real(4) :: wdlt, wmaxdlt

    fg_xi = 1 - itgtaxs
    fg_eta = itgtaxs
    ilen = fg_eta*lklen + fg_xi*ljlen
    allocate(wdat(ilen+1))
    allocate(wcod(ilen+1))
    allocate(fg_valid(ilen))
    fg_valid = 1
    wcod(1) = sgrd(1, fg_eta*itgt + fg_xi, fg_eta + fg_xi*itgt, 1)
    do i1=2, ilen
      wcod(i1) = sgrd(1, fg_eta*itgt + fg_xi*i1, fg_eta*i1 + fg_xi*itgt, 1)
      if(wcod(i1) == wcod(i1-1)) fg_valid(i1) = 0
    enddo
    wcod(ilen+1) = wcod(2) + wcod(ilen)
    if(wcod(2) == wcod(ilen)) fg_valid(ilen) = 0
    do i1=1, lscl
      sout(1, i1) = sfld(ltimidx)
      do i2=1, ilen
        wdat(i2) = sscl(fg_eta*itgt + fg_xi*i2, fg_eta*i2 + fg_xi*itgt, 1, lscllst(i1))
        if(wdat(i2) == 0.d0) fg_valid(i2) = 0
      enddo
      wdat(ilen+1) = wdat(2)
      wmaxdlt = 0.d0
      imaxdltidx = 1
      do i2=1, ilen
        if(fg_valid(i2) == 0) continue
        wdlt = wdat(i2) - wdat(i2+1)
        wdlt = (wdlt*ishkdir + (1-abs(ishkdir))*abs(wdlt)) / abs(wcod(i2+1) - wcod(i2))
        if(wmaxdlt < wdlt)then
          imaxdltidx = i2
          wmaxdlt = wdlt
        endif
      enddo
      if(imaxdltidx == ilen) imaxdltidx = 1
      sout(2, i1) = imaxdltidx
      sout(3, i1) = wcod(imaxdltidx)
      sout(4, i1) = wcod(imaxdltidx+1)
      sout(5, i1) = wdat(imaxdltidx)
      sout(6, i1) = wdat(imaxdltidx+1)
    enddo

  endsubroutine

end module
