program postprocess
use mod_VTK_IO
use mod_szk
use mod_globals
use prc_searchShock

implicit none

10 format(A40, 100(',', A40))
11 format(e14.6, 100(',', e14.6))


!==================================ここからメインプログラム==================================

call readArgs
call setSearchFlowFiles
call setCaseName
hFileList = openFileListStream(ffilter)
  write(lwrite,*) 'File list created.'
  call selectFlowInfo
  call selectProcess
  select case (lprc)
    case (1)
      !shockSearch
      call setSearchAxis
      call setShockDirection

      call readyForCalc
      do
        call mainloop
      enddo
    case default
      write(lwrite, *) 'Invalid procedure...'
  end select
  if(lread /=5) lread=close2(lread)

!==================================ここまでメインプログラム==================================











!==================================CALC==================================





do



  write(6, *) ifname

!$OMP PARALLEL DO PRIVATE(i)
  do i=1,fncnum
    hOut = lock2()
    open(unit=hOut, file=trim(oname)//exts(i), form='formatted', position='append')
    write(hOut, 11) (dat(j,i), j=1, 6)
    hOut = close2(hOut)
  enddo
!$OMP END PARALLEL DO
enddo
999 hFileList = closeFindListStream(hFileList)






contains

  subroutine readArgs
    integer :: ifilnamlen
    character(:), allocatable :: cfilnam
    if(command_argument_count() > 0)then
      call get_command_argument(1, length=ifilnamlen)
      allocate(character(ibuf) :: cfilnam)
      call get_command_argument(1, cfilnam)
      lread = open2(file=cfilnam, form='formatted', status='old')
      deallocate(cfilnam)
    else
      lread = 5
    endif
  endsubroutine

  subroutine setSearchFlowFiles
    character(*), parameter :: cdeffilcmd = 'ls | egrep "^#flow" | egrep -v "#flow.vts"'
    character(100) :: cfilcmd
    write(lwrite,*) 'Use default file search command: ' // cdeffilcmd // ' ?'
    write(lwrite,*) 'If not, input something excutable to find file.'
    read(lread,'(a)') cfilcmd
    if(len_trim(cfilcmd) < 2)then
      allocate(character(len(cdeffilcmd)) :: bfilcmd)
      bfilcmd = cdeffilcmd
    else
      allocate(character(len_trim(cfilcmd)) :: bfilcmd)
      bfilcmd = trim(cfilcmd)
    endif
    write(lwrite,*) 'file search command: ' // bfilcmd
  endsubroutine

  subroutine setCaseName
    character(300) :: cdefoutpfx
    character(100) :: coutpfx
    character(*), parameter :: cdefksk = '_kaiseki'
    integer :: iidx
    call getcwd(cdefoutpfx)
    iidx = 1
    do
      cdefoutpfx = split(cdefoutpfx, '/', iidx)
      if (iidx == 0) exit
    enddo
    cdefoutpfx = cdefoutpfx // cdefksk

    write(lwrite,*) 'Input name of this case.'
    write(lwrite,*) 'Output file name is ***.csv, ***_info.txt, ...etc'
    write(lwrite,*) 'Press only "Enter" key to use default, "' // trim(cdefoutpfx) //'".'
    read(lread,'(a)') coutpfx
    if(len_trim(coutpfx) < 2)then
      allocate(character(len(cdefoutpfx)) :: boutpfx)
      boutpfx = cdefoutpfx
    else
      allocate(character(len_trim(coutpfx)) :: boutpfx)
      boutpfx = trim(coutpfx)
    endif
    write(lwrite,*) 'case name: ' // boutpfx
  endsubroutine

  function trimMid(estr)
    character(*), intent(in) :: estr
    character(:), allocatable :: trimMid
    character(1), parameter :: cspc = ''
    integer :: ilen, i1, i2
    ilen = len(estr)
    allocate(character(ilen) :: trimMid)
    trimMid = cspc
    i2 = 1
    do i1=1, ilen
      if(estr(i1:i1) /= cspc)then
        trimMid(i2:i2) = estr(i1:i1)
        i2 = i2 + 1
      endif
    enddo
  endfunction

  subroutine selectFlowInfo
    character(300) :: cflwfil
    integer :: hFlow, hInfo
    integer :: ifld, ivct, iscl
    character(30), allocatable :: cfldlst(:), cvctlst(:), cscllst(:), cinplst(:)
    character(30) :: cinp
    integer, allocatable :: iftplst
    character(*), parameter :: cinfsfx = '_info.csv'
    integer :: fg_err
    integer :: iidx, i1

    !Get Grid info
    read(hFileList,*, end=999) cflwfil
    rewind(hFileList)
    write(lwrite,*) 'Getting flowfile format from '//trim(cflwfil)
    hFlow = lock2()
      call VTK_Reader_GetNumberOfExtent(cflwfil, hFlow, lwrite, ljbgn, ljend, lkbgn, lkend, llbgn, llend)
      call VTK_Reader_GetNumberOfFunctions(cflwfil, hFlow, lwrite, ifld, ivct, iscl)
      allocate(cfldlst(ifld))
      allocate(cvctlst(ivct))
      allocate(cscllst(iscl))
      allocate(iftplst(ifld))
      call VTK_Reader_GetNumberOfTuples(cflwfil, hFlow, lwrite, ifld, iftplst)
      call VTK_Reader_GetFunctionNames(cflwfil, hFlow, lwrite, ifld, ivct, iscl, cfldlst, cvctlst, cscllst)
    hFlow = release2(hFlow)

    !Get FlowFile Functions
    hInfo = lock2()
      ljlen = ljend - ljbgn + 1
      lklen = lkend - lkbgn + 1
      lllen = llend - llbgn + 1
      ljklsum = ljlen * lklen * lllen
      fg_err = 1
      open(unit=hInfo, file=boutpfx//cinfsfx, form='formatted', status='replace', err=200)
      write(hInfo, *) 'Sum of grids,' // toString(ljklsum)
      write(hInfo, *) 'Grid Info (xi-eta-zeta),' // toString(ljlen) // ',' // toString(lklen) // ',' // toString(lllen)
      write(hInfo, *) 'Number of Field data,' // toString(ifld)
      write(hInfo, 10) 'Field data,', (cfldlst(iidx), iidx=1, ifld)
      write(hInfo, 10) 'Size of field data tuples,', (toString(iftplst(iidx)), iidx=1, ifld)
      write(hInfo, *) 'Number of scalars,' // toString(iscl)
      write(hInfo, 10) 'Scalar data,', (cscllst(iidx), iidx=1, iscl)
      write(hInfo, *) 'Number of vectors,' // toString(ivct)
      write(hInfo, 10) 'Vecor data,', (cvctlst(iidx), iidx=1, ivct)
      hInfo = close2(hInfo)
      hInfo = 0
      fg_err = 0
200   if(fg_err == 0)then
        write(lwrite,*) 'Error! Cannot save info file!'
        stop
      endif
      write(lwrite, *) 'Completed getting format of flowfile. Output files are:'
      write(lwrite,*) boutpfx//cinfsfx
    if(hInfo/=0) hInfo=release2(hInfo)
    write(lwrite, *) ' '
    write(lwrite, *) 'Grid Points (xi, eta, zeta),' // toString(ljlen) // ',' // toString(lklen) // ',' // toString(lllen)

    !Choose Functions
    do i1=1, iscl
      write(lwrite, *) trim(toString(i1)//' : '//cscllst(i1))
    enddo
    write(lwrite,*) 'Which scalar data do you want?'
    write(lwrite,*) 'e.g.) 1-2-4 : Output No.1, No2 and No4 data.'
    read(lread,*) cinp
    cinp = trimMid(cinp)
    lscl = countstr(trim(cinp), '-') + 1
    allocate(cinplst(lscl))
    iidx = 1
    do i1=1, lscl
      cinplst(iidx) = split(trim(cinp), '-', iidx)
    enddo
    allocate(lscllst(lscl))
    allocate(bscllst(lscl))
    allocate(boutsfx(lscl))
    write(lwrite,*) 'Scalar data to output are:'
    write(lwrite,*) toString(lscl)//'data set(s)'
    do i1=1, lscl
      lscllst(i1) = toInteger(cinplst(i1))
      bscllst(i1) = cscllst(lscllst(i1))
      boutsfx(i1) = '_' // trimMid(bscllst(i1)) // '.csv'
      write(lwrite,*) trim(adjustl(bscllst(i1)))
    enddo
    write(lwrite, *) ' '
    write(lwrite, *) 'Field data are'
    do i1=1, ifld
      write(lwrite, *) trim(toString(i1) // ' : ' // cfldlst(i1))
    enddo
    write(lwrite,*) 'Which field data do you use as time?'
    read(lread,*) ltimidx
  endsubroutine

  subroutine selectProcess
    integer, parameter :: iprc = size(bprclst)
    integer :: iidx
    write(lwrite, *) 'Available procedures of post process are:'
    do iidx=1, iprc
      write(lwrite, *) trim(toString(iidx) // ' : ' // bprclst(iidx))
    enddo
    write(lwrite,*) 'Which procedure do you use?'
    read(lread, *) lprc
  endsubroutine

  subroutine readyForCalc
    integer :: i1
    write(lread,*) 'Now deleting old files...'
    do i1=1, lscl
      call system('rm '//trim(boutpfx//boutsfx))
    enddo
    allocate(wfld(ifld))
    allocate(wvct(3, ljlen, lklen, lllen, lvct))
    allocate(wscl(ljlen, lklen, lllen, lscl))
    allocate(wgrd(3, ljlen, lklen, lllen))
    allocate(dat(lprcoutcol(lprc), lscl))
  endsubroutine

  subroutine mainloop
    character(300) :: cflwfil
    integer :: hFlow
    character(*), parameter :: cdbg = '    '
    read(hFileList,*, end=999) iflwfil
    hFlow = lock2()
    call VTK_Reader_StructuredGrid(iflwfil, hFlow, lwrite, cdbg,&
&    flst, vlst, slst, ifld, ivct, iscl,&
  & jbgn, jend, kbgn, kend, lbgn, lend,&
  & iftp, wfld, wvct, wscl, wgrd)
    hFile = release2(hFile)
    dat = searchShock(fncnum, fg_tgt-1, tgt, idxtime, shockdir,&
  & wfld, wscl, wgrd, jbgn, jend, kbgn, kend)
  endsubroutine

end program
