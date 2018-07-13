program postprocess
use mod_VTK_IO
use mod_szk
use mod_globals
use prc_searchShock
use prc_shockHeight

implicit none


!==================================ここからメインプログラム==================================

call readArgs
write(lwrite, *) 'Hello, this is ppszk!!'
write(lwrite, *) ' '
call setSearchFlowFiles
call setCaseName
hFileList = openFileListStream(bfilcmd)
  write(lwrite,*) 'File list created.'
  call selectFlowInfo
  call selectProcess
  select case (lprc)

!==========================ここから処理(1)
    case (1)
      !========ここから可変
      call setSearchAxis
      call setShockDirection
      !========ここまで可変

      call readyForCalc
      fg_loop = 0
      do while (fg_loop == 0)
        call input
        !========ここから可変
        call searchShock
        !========ここまで可変
        call output
      enddo
!==========================ここまで処理(1)

!==========================ここから処理(2)
    case (2)
      !========ここから可変
      call setThreshold
      call setInitial
      !========ここまで可変

      call readyForCalc
      fg_loop = 0
      do while (fg_loop == 0)
        call input
        !========ここから可変
        call shockHeight
        !========ここまで可変
        call output
      enddo
!==========================ここまで処理(2)

    case default
      write(lwrite, *) 'Invalid procedure...'
      hFileList = closeFindListStream(hFileList)
      stop

  end select
100 hFileList = closeFindListStream(hFileList)
if(lread /=5) lread=close2(lread)
write(lwrite, *) 'MISSION ACCOMPLISHED !!!!'
stop

!==================================ここまでメインプログラム==================================

contains

!==================================ここからサブルーチン======================================

  function toFileName(a)
    character(*), intent(in) :: a
    character(3) :: cbuf, toFileName
    integer ::  i
    cbuf = (trim(a) // '___')
    toFileName = ''
    do i=1,3
      if(cbuf(i:i) == '[' .or. cbuf(i:i) == ']')then
        toFileName = trim(toFileName) // '-'
      else
        toFileName = trim(toFileName) // cbuf(i:i)
      endif
    enddo
  endfunction

  subroutine readArgs
    integer :: ifilnamlen
    character(:), allocatable :: cfilnam
    if(command_argument_count() > 0)then
      call get_command_argument(1, length=ifilnamlen)
      allocate(character(ifilnamlen) :: cfilnam)
      call get_command_argument(1, cfilnam)
      lread = open2(file=cfilnam, form='formatted', status='old')
    else
      lread = 5
    endif
  endsubroutine

  subroutine setSearchFlowFiles
    character(*), parameter :: cdeffilcmd = 'ls | egrep "^#flow" | egrep -v "#flow.vts"'
    character(100) :: cfilcmd
    write(lwrite,*) 'If you use default file search command, press only "Enter" key.'
    write(lwrite,*) 'default command: ' // cdeffilcmd
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
    write(lwrite, *) ' '
  endsubroutine

  subroutine setCaseName
    character(300) :: cdefoutpfx, cdir
    character(100) :: coutpfx
    character(*), parameter :: cdefksk = '_kaiseki'
    integer :: iidx
    call getcwd(cdefoutpfx)
    iidx = 1
    do
      cdir = split(trim(cdefoutpfx), '/', iidx)
      if (iidx == 0) exit
    enddo
    cdefoutpfx = trim(cdir) // cdefksk

    write(lwrite,*) 'Input name of this case!'
    write(lwrite,*) 'Output file name is ***_Pre.csv, ***_info.csv, ...etc'
    write(lwrite,*) 'Press only "Enter" key to use default, "' // trim(cdefoutpfx) //'".'
    read(lread,'(a)') coutpfx
    if(len_trim(coutpfx) < 2)then
      allocate(character(len_trim(cdefoutpfx)) :: boutpfx)
      boutpfx = trim(cdefoutpfx)
    else
      allocate(character(len_trim(coutpfx)) :: boutpfx)
      boutpfx = trim(coutpfx)
    endif
    write(lwrite,*) 'case name: ' // boutpfx
    write(lwrite, *) ' '
  endsubroutine

  subroutine selectFlowInfo
    character(300) :: cflwfil
    integer :: hFlow, hInfo
    character(30), allocatable :: cinplst(:)
    character(30) :: cinp
    character(*), parameter :: cinfsfx = '_info.csv'
    integer :: fg_err
    integer :: iidx, i1

201 format(A40, 100(',', A40))

    !Get Grid info
    read(hFileList,*) cflwfil
    rewind(hFileList)
    write(lwrite,*) 'Getting flowfile format from "'//trim(cflwfil)//'"'
    hFlow = lock2()
      call VTK_Reader_GetNumberOfExtent(cflwfil, hFlow, lwrite, ljbgn, ljend, lkbgn, lkend, llbgn, llend)
      call VTK_Reader_GetNumberOfFunctions(cflwfil, hFlow, lwrite, lallfld, lallvct, lallscl)
      allocate(ballfldlst(lallfld))
      allocate(ballvctlst(lallvct))
      allocate(ballscllst(lallscl))
      allocate(lallftplst(lallfld))
      call VTK_Reader_GetNumberOfTuples(cflwfil, hFlow, lwrite, lallfld, lallftplst)
      call VTK_Reader_GetFunctionNames(cflwfil, hFlow, lwrite, lallfld, lallvct, lallscl, ballfldlst, ballvctlst, ballscllst)
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
      write(hInfo, *) 'Number of Field data,' // toString(lallfld)
      write(hInfo, 201) 'Field data,', (ballfldlst(iidx), iidx=1, lallfld)
      write(hInfo, 201) 'Size of field data tuples,', (toString(lallftplst(iidx)), iidx=1, lallfld)
      write(hInfo, *) 'Number of scalars,' // toString(lallscl)
      write(hInfo, 201) 'Scalar data,', (ballscllst(iidx), iidx=1, lallscl)
      write(hInfo, *) 'Number of vectors,' // toString(lallvct)
      write(hInfo, 201) 'Vecor data,', (ballvctlst(iidx), iidx=1, lallvct)
      hInfo = close2(hInfo)
      hInfo = 0
      fg_err = 0
200   if(fg_err == 1)then
        write(lwrite,*) 'Error! Cannot save info file!'
        write(lwrite, *) ' '
        stop
      endif
      write(lwrite, *) 'Completed getting format of flowfile. Output files are:'
      write(lwrite,*) boutpfx//cinfsfx
    if(hInfo/=0) hInfo=release2(hInfo)
    write(lwrite, *) ' '
    write(lwrite, *) 'Grid Points (xi, eta, zeta),' // toString(ljlen) // ',' // toString(lklen) // ',' // toString(lllen)

    !Choose Functions
    do i1=1, lallscl
      write(lwrite, *) trim(toString(i1)//' : '//ballscllst(i1))
    enddo
    write(lwrite,*) 'Which scalar data do you want?'
    write(lwrite,*) 'e.g.) 1-2-4 : Output No.1, No2 and No4 data.'
    read(lread,*) cinp
    cinp = deleteSpace(cinp)
    lscl = countstr(trim(cinp), '-') + 1
    allocate(cinplst(lscl))
    iidx = 1
    do i1=1, lscl
      cinplst(i1) = split(trim(cinp), '-', iidx)
    enddo
    allocate(lscllst(lscl))
    allocate(bscllst(lscl))
    allocate(boutsfx(lscl))
    write(lwrite,*) 'Scalar data to output are:'
    write(lwrite,*) toString(lscl)//'data set(s)'
    do i1=1, lscl
      lscllst(i1) = toInteger(cinplst(i1))
      bscllst(i1) = ballscllst(lscllst(i1))
      boutsfx(i1) = '_' // toFileName(deleteSpace(bscllst(i1))) // '.csv'
      write(lwrite,*) atrim(bscllst(i1))
    enddo
    write(lwrite, *) ' '
    write(lwrite, *) 'Field data are'
    do i1=1, lallfld
      write(lwrite, *) trim(toString(i1) // ' : ' // ballfldlst(i1))
    enddo
    write(lwrite,*) 'Which field data do you use as time?'
    read(lread,*) ltimidx
    write(lwrite,*) 'Use as time: '
    write(lwrite,*) ballfldlst(ltimidx)
    write(lwrite, *) ' '
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
    write(lwrite,*) 'Procedure: '
    write(lwrite,*) bprclst(lprc)
    write(lwrite, *) ' '
    write(lwrite, *) 'Now, entering user procedure...'
    write(lwrite, *) ' '
  endsubroutine

  subroutine readyForCalc
    integer :: i1
    write(lwrite,*) 'Now deleting old files...'
    do i1=1, lscl
      call system('rm '//trim(boutpfx//boutsfx(i1)))
    enddo
    allocate(sfld(lallfld))
    allocate(lftp(lfld))
    allocate(svct(3, ljlen, lklen, lllen, lallvct))
    allocate(sscl(ljlen, lklen, lllen, lallscl))
    allocate(sgrd(3, ljlen, lklen, lllen))
    allocate(sout(lprcoutcol(lprc), lscl))
    write(lwrite,*) 'Start main loop...'
  endsubroutine

  subroutine input
    character(300) :: cflwfil
    integer :: hFlow
    character(*), parameter :: cdbg = '    '
    read(hFileList,*, end=210) cflwfil
    write(lwrite, *) cflwfil
    hFlow = lock2()
    call VTK_Reader_StructuredGrid(cflwfil, hFlow, lwrite, cdbg,&
&    ballfldlst, ballvctlst, ballscllst, lallfld, lallvct, lallscl,&
&    ljbgn, ljend, lkbgn, lkend, llbgn, llend, lallftplst, sfld, svct, sscl, sgrd)
    hFlow = release2(hFlow)
    return
210 fg_loop = 1
  endsubroutine

  subroutine output
    integer :: i1, i2
    integer :: hOut

202 format(e14.6, 100(',', e14.6))

    do i1=1, lscl
      hOut = lock2()
      open(unit=hOut, file=boutpfx//boutsfx(i1), form='formatted', position='append')
      write(hOut, 202) (sout(i2,i1), i2=1, lprcoutcol(lprc))
      hOut = close2(hOut)
    enddo
  endsubroutine

!==================================ここまでサブルーチン======================================

end program
