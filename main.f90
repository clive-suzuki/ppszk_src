program postprocess
use mod_VTK_IO
use mod_szk
use mod_globals


implicit none




!VTK読み込み
integer :: fg_noncalc = 0

real(4), allocatable :: wfld(:), wvct(:,:,:,:,:), wscl(:,:,:,:), wgrd(:,:,:,:)



real(4), allocatable :: dat(:,:)



!コマンドライン
character(:), allocatable :: arg_com


10 format(A40, 100(',', A40))
11 format(e14.6, 100(',', e14.6))
!==================================INPUT==================================

call readArgs
call setSearchFlowFiles
call setCaseName
hFileList = openFileListStream(ffilter)
  write(lwrite,*) 'File list created.'
  call selectFlowInfo
  call selectProcess
  select case (lprc)
    case (1)
      !
    case default
      write(lwrite, *) 'Invalid procedure...'
  end select


do
  write(6,*) 'Which direction do you want? (1: xi / 2: eta)'
  read(lread, *) fg_tgt
  if(fg_tgt==1 .or. fg_tgt==2) exit
enddo
write(6,*) 'Target coord?'
read(lread, *) tgt


write(6,*) 'Shock direction? ( -1/ 1 ) If not, prease enter 0.'
read(lread,*) shockdir


if(lread /=5) lread=close2(lread)


!==================================CALC==================================

allocate(wfld(ifld))
allocate(wvct(3, jj, kk, ll, ivct))
allocate(wscl(jj, kk, ll, iscl))
allocate(wgrd(3, jj, kk, ll))
allocate(dat(6,fncnum))

write(6,*) 'Calcuration start. Output files are:'
do i=1, fncnum
  write(6,*) trim(oname)//exts(i)
  call system('rm '//trim(oname)//exts(i))
enddo

do
  read(hFileList,*, end=999) ifname
  hFile = lock2()
  call VTK_Reader_StructuredGrid(&
& ifname, hFile, 6, '    ',&
& flst, vlst, slst, ifld, ivct, iscl,&
& jbgn, jend, kbgn, kend, lbgn, lend,&
& iftp, wfld, wvct, wscl, wgrd)
  hFile = release2(hFile)
  dat = searchShock(fncnum, fg_tgt-1, tgt, idxtime, shockdir,&
& wfld, wscl, wgrd, jbgn, jend, kbgn, kend)

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
    character(30), parameter :: cprclst = (/ 'searchShock' /)
    integer, parameter :: iprc = size(cprclst)
    integer :: iidx
    write(lwrite, *) 'Available procedures of post process are:'
    do iidx=1, iprc
      write(lwrite, *) trim(toString(iidx) // ' : ' // cprclst(iidx))
    enddo
    write(lwrite,*) 'Which procedure do you use?'
    read(lread, *) lprc
  endsubroutine


  function searchShock(fncnum, fg_tgt2, tgt, idxtime, shc, wfld, wscl, wgrd,&
& js, je, ks, ke)
    integer, intent(in) :: fg_tgt2, tgt, idxtime, fncnum, shc
    integer, intent(in) :: js, je, ks, ke
    real(4), allocatable :: wfld(:), wscl(:,:,:,:), wgrd(:,:,:,:)
    real(4) :: searchShock(6, fncnum)
    integer :: i, j, x
    integer :: maxx
    real(4), allocatable :: dat(:), cod(:)
    real(4) :: x_maxrt, rt_max, rt, rtbuf

    maxx = fg_tgt2*(ke-ks+1)-(fg_tgt2-1)*(je-js+1)
    allocate(dat(maxx+1))
    allocate(cod(maxx+1))

    do i=1, fncnum
      searchShock(1, i) = wfld(idxtime)
      select case (fg_tgt2)
      case(0)
!$OMP PARALLEL DO PRIVATE(j)
          do j=1,maxx
            dat(j) = wscl(j, tgt, 1, ifnclist(i))
            cod(j) = wgrd(1, j, tgt, 1)
          enddo
!$OMP END PARALLEL DO
          dat(maxx+1) = wscl(2, tgt, 1, ifnclist(i))
          cod(maxx+1) = wgrd(2, j, tgt, 1) + cod(maxx)
        case(1)
!$OMP PARALLEL DO PRIVATE(j)
          do j=1,maxx
            dat(j) = wscl(tgt, j, 1, ifnclist(i))
            cod(j) = wgrd(1, tgt, j, 1)
          enddo
!$OMP END PARALLEL DO
          dat(maxx+1) = wscl(tgt, 2, 1, ifnclist(i))
          cod(maxx+1) = wgrd(2, tgt, 1, 1) + cod(maxx)
      end select
      rt_max = 0
      do x=1, maxx
        if(dat(x)*dat(x+1)*(cod(x+1)-cod(x)) == 0.d0) continue
        rtbuf = dat(x)/dat(x+1)
        rt = ((rtbuf)**shc + (1-abs(shc))*max(rtbuf, 1.d0/rtbuf)) / abs(cod(x+1)-cod(x))
        if(rt_max<rt)then
          x_maxrt = x
          rt_max = rt
        endif
      enddo
      if(x_maxrt == maxx+1) x_maxrt = 1
      searchShock(2, i) = x_maxrt
      searchShock(3, i) = cod(x_maxrt)
      searchShock(4, i) = cod(x_maxrt+1)
      searchShock(5, i) = dat(x_maxrt)
      searchShock(6, i) = dat(x_maxrt+1)
    enddo
  endfunction

end program
