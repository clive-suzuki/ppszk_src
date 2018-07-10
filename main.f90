program postprocess
use mod_VTK_IO
use mod_szk


implicit none

!汎用
character(100) :: sbuf
character(100), allocatable :: sbuflist(:)
integer :: i, j, k, l
integer :: ibuf
integer :: jj, kk, ll !grid


!IO
integer :: lread
character(100) :: ifname, cname, oname, iname
character(*), parameter :: def_ffilter = 'ls | egrep "^#flow" | egrep -v "#flow.vts"'
character(100) :: ffilter
integer :: hFileList, hFile, hInfo, hOut
character(8), allocatable :: exts(:)


!VTK読み込み
integer :: fg_noncalc = 0
integer :: jbgn, jend, kbgn, kend, lbgn, lend
integer :: ifld, ivct, iscl, idxtime
character(30), allocatable :: flst(:), vlst(:), slst(:)
integer, allocatable :: iftp(:)
real(4), allocatable :: wfld(:), wvct(:,:,:,:,:), wscl(:,:,:,:), wgrd(:,:,:,:)
integer :: fncnum
character(30), allocatable :: sfnclist(:)
integer, allocatable :: ifnclist(:)
integer :: shockdir
integer :: fg_tgt, tgt
real(4), allocatable :: dat(:,:)



!コマンドライン
character(:), allocatable :: arg_com


10 format(A40, 100(',', A40))
11 format(e14.6, 100(',', e14.6))
!==================================INPUT==================================

if(command_argument_count() > 0)then
  call get_command_argument(1, length=ibuf)
  allocate(character(ibuf) :: arg_com)
  call get_command_argument(1, arg_com)
  sbuf = arg_com
else
  write(6,*) 'Input from com?'
  write(6,*) 'If not, press only "Enter" key to go next.'
  read(5,'(a)') sbuf
endif
if(len_trim(sbuf)==0)then
  lread = 5
else
  lread = open2(file=trim(sbuf), form='formatted', status='old')
endif

write(6,*) 'Use default file search command: ' // def_ffilter // ' ?'
write(6,*) 'If not, input something excutable to find file.'
read(lread,'(a)') ffilter
if(len_trim(ffilter)<2)then
  ffilter = def_ffilter
endif
write(6,*) 'filter: '//ffilter

write(6,*) 'Input name of this case.'
write(6,*) 'Output file name is ***.csv, ***_info.txt, ...etc'
write(6,*) 'Press only "Enter" key to use default, "kaiseki".'
read(lread,'(a)') cname
if(len_trim(cname)<2)then
  cname = "kaiseki"
endif
iname = trim(cname) // '_info.csv'
oname = trim(cname)
write(6,*) 'case name: '//cname

hFileList = openFileListStream(ffilter)
write(6,*) 'File list created.'
read(hFileList,*, end=999) ifname
hFile = lock2()
rewind(hFileList)
write(6,*) trim(ifname)

call VTK_Reader_GetNumberOfExtent( &
&  ifname, hFile, 6, jbgn, jend, kbgn, kend, lbgn, lend)

call VTK_Reader_GetNumberOfFunctions( &
&  ifname, hFile, 6, ifld, ivct, iscl)

write(6,*) 'Getting grid information...'

allocate(flst(ifld))
allocate(vlst(ivct))
allocate(slst(iscl))
allocate(iftp(ifld))

call VTK_Reader_GetNumberOfTuples(&
&  ifname, hFile, 6, ifld, iftp)

call VTK_Reader_GetFunctionNames(ifname, hFile, 6, &
&  ifld, ivct, iscl, flst, vlst, slst)

hFile = release2(hFile)

hInfo = lock2()
jj = jend-jbgn+1
kk = kend-kbgn+1
ll = lend-lbgn+1
open(unit=hInfo, file=iname, form='formatted', status='replace', err=100)
write(hInfo, *) 'Sum of grids,' // toString(jj*kk*ll)
write(hInfo, *) 'Grid Info (xi-eta-zeta),' // toString(jj) // ',' // toString(kk) // ',' // toString(ll)
write(hInfo, *) 'Number of Field data,' // toString(ifld)
write(hInfo, 10) 'Field data,', (flst(i), i=1, ifld)
write(hInfo, 10) 'Size of field data tuples,', (toString(iftp(i)), i=1, ifld)
write(hInfo, *) 'Number of scalars,' // toString(iscl)
write(hInfo, 10) 'Scalar data,', (slst(i), i=1, iscl)
write(hInfo, *) 'Number of vectors,' // toString(ivct)
write(hInfo, 10) 'Vecor data,', (vlst(i), i=1, ivct)
hInfo = close2(hInfo)
hInfo = 0
100 write(6, *) 'Completed getting infomation of grids. Output files are:'
write(6,*) iname
if(hInfo/=0) hInfo=release2(hInfo)
write(6, *) ' '
write(6, *) 'Grid Info (xi, eta, zeta),' // toString(jj) // ',' // toString(kk) // ',' // toString(ll)

do
  write(6,*) 'Which direction do you want? (1: xi / 2: eta)'
  read(lread, *) fg_tgt
  if(fg_tgt==1 .or. fg_tgt==2) exit
enddo
write(6,*) 'Target coord?'
read(lread, *) tgt

do i=1, iscl
  write(6, *) trim(toString(i) // ' : ' // slst(i))
enddo
write(6,*) 'What scalar data do you want?'
write(6,*) 'e.g.) 1-2-4 : Output No.1, No2 and No4 data.'
read(lread,*) sbuf

sbuf = adjustl(sbuf)

fncnum = countstr(trim(sbuf), '-', len_trim(sbuf),1) +1
allocate(sbuflist(fncnum))
i = 1
j = 1
do i=1, fncnum
  sbuflist(i) = split(trim(sbuf), '-', j)
enddo
allocate(ifnclist(fncnum))
allocate(sfnclist(fncnum))
allocate(exts(fncnum))
write(6,*) 'Scalar data to output are:'
write(6,*) toString(fncnum)//'data set(s)'
do i=1, fncnum
  ifnclist(i) = toInteger(sbuflist(i))
  sbuf = slst(ifnclist(i))
  sfnclist(i) = trim(sbuf)
  sbuf = adjustl(sbuf)
  write(6,*) sbuf
  exts(i) = '_'//sbuf(1:3)//'.csv'
enddo

do i=1, ifld
  write(6, *) trim(toString(i) // ' : ' // flst(i))
enddo
write(6,*) 'What field data do you use as time?'
read(lread,*) idxtime

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
