module mod_VTK_IO

  implicit none

  integer, parameter, private :: lth = 30000 ! character length for cdata
  integer, parameter, private :: rs  = 0     ! root MPI node

interface VTK_Reader_StructuredGrid
  module procedure VTK_Reader_StructuredGrid_Single,                          &
  &                VTK_Reader_StructuredGrid_Double
end interface VTK_Reader_StructuredGrid

interface VTK_Reader_UnstructuredGrid
  module procedure VTK_Reader_UnstructuredGrid_Single,                        &
  &                VTK_Reader_UnstructuredGrid_Double
  end interface VTK_Reader_UnstructuredGrid

interface VTK_Writer_StructuredGrid
  module procedure VTK_Writer_StructuredGrid_Single,                          &
  &                VTK_Writer_StructuredGrid_Double
end interface VTK_Writer_StructuredGrid

interface VTK_Writer_PStructuredGrid
  module procedure VTK_Writer_PStructuredGrid_Single
end interface VTK_Writer_PStructuredGrid

interface VTK_Writer_UnstructuredGrid
  module procedure VTK_Writer_UnstructuredGrid_Single,                        &
  &                VTK_Writer_UnstructuredGrid_Double
end interface VTK_Writer_UnstructuredGrid

interface VTK_Writer_PUnstructuredGrid
  module procedure VTK_Writer_PUnstructuredGrid_Single
end interface VTK_Writer_PUnstructuredGrid

interface VTK_Writer_ImageData
  module procedure VTK_Writer_ImageData_Single,                               &
  &                VTK_Writer_ImageData_Double
end interface VTK_Writer_ImageData

interface VTK_Writer_PImageData
  module procedure VTK_Writer_PImageData_Single
end interface VTK_Writer_PImageData

!=================================================
! Code desctiption
!
! VTK_StructuredGrid_Reader(cnme,iop,iow,                          &
!                           flst,vlst,slst,ifld,ivct,iscl,         &
!                           jbgn,jend,kbgn,kend,lbgn,lend,         &
!                           iftp,wfld,wvct,wscl,wgrd)
!   Read All data (function name, grid points, arrays) for StructureGrid
!
!   (IN)  cnme            [character(100)] : read file name
!   (IN)  iop             [integer]        : file unit number for IO
!   (IN)  iow             [integer]        : file unit number for error message
!   (IN)  flst(:)         [character(30)]  : name list for field  function
!   (IN)  vlst(:)         [character(30)]  : name list for vector function
!   (IN)  slst(:)         [character(30)]  : name list for scalar function
!   (IN)  ifld            [integer]        : number of field  functions
!   (IN)  ivct            [integer]        : number of vector functions
!   (IN)  iscl            [integer]        : number of scalar functions
!   (IN)  bgn,end         [integer]        : grid point
!   (IN)  iftp(:)         [integer]        : number of tuples for field array
!   (OUT) wfld(:)         [real(4,8)]      : field data
!   (OUT) wvct(:,:,:,:,:) [real(4,8)]      : vector data
!   (OUT) wscl(:,:,:,:)   [real(4,8)]      : scalar data
!   (OUT) wgrd(:,:,:,:)   [real(4,8)]      : grid data
!
! VTK_Writer_StructuredGrid(cnme,cpnt,iop,                        &
!                           flst,vlst,slst,ifld,ivct,iscl,        &
!                           jbgn,jend,kbgn,kend,lbgn,lend,        &
!                           iftp,wfld,wvct,wscl,wgrd)
!   Write All data (function name, grid points, arrays) for StructureGrid
!
!   (IN)  cnme            [character(100)] : read file name
!   (IN)  cpnt            [character(50)]  : whole extent (for MPI parallel)
!   (IN)  iop             [integer]        : file unit number for IO
!   (IN)  flst(:)         [character(30)]  : name list for field  function
!   (IN)  vlst(:)         [character(30)]  : name list for vector function
!   (IN)  slst(:)         [character(30)]  : name list for scalar function
!   (IN)  ifld            [integer]        : number of field  functions
!   (IN)  ivct            [integer]        : number of vector functions
!   (IN)  iscl            [integer]        : number of scalar functions
!   (IN)  bgn,end         [integer]        : grid point
!   (IN)  iftp(:)         [integer]        : number of tuples for field array
!   (IN)  wfld(:)         [real(4,8)]      : field data
!   (IN)  wvct(:,:,:,:,:) [real(4,8)]      : vector date
!   (IN)  wscl(:,:,:,:)   [real(4,8)]      : scalar data
!   (IN)  wgrd(:,:,:,:)   [real(4,8)]      : grid data
!
! VTK_UnstructuredGrid_Reader(cnme,iop,iow,                      &
!                             flst,vlst,slst,ifld,ivct,iscl,     &
!                             nmax,                              &
!                             iftp,wfld,wvct,wscl,wgrd)
!   Read All data (function name, grid points, arrays) for UnstructureGrid
!
!   (IN)  cnme            [character(100)] : read file name
!   (IN)  iop             [integer]        : file unit number for IO
!   (IN)  iow             [integer]        : file unit number for error message
!   (IN)  flst(:)         [character(30)]  : name list for field  function
!   (IN)  vlst(:)         [character(30)]  : name list for vector function
!   (IN)  slst(:)         [character(30)]  : name list for scalar function
!   (IN)  ifld            [integer]        : number of field  functions
!   (IN)  ivct            [integer]        : number of vector functions
!   (IN)  iscl            [integer]        : number of scalar functions
!   (IN)  nmax            [integer]        : grid point
!   (IN)  iftp(:)         [integer]        : number of tuples for field array
!   (OUT) wfld(:)         [real(4)]        : field data
!   (OUT) wvct(:,:,:)     [real(4)]        : vector date
!   (OUT) wscl(:,:)       [real(4)]        : scalar data
!   (OUT) wgrd(:,:)       [real(4)]        : grid data
!
! VTK_Writer_UnstructuredGrid(cnme,iop,                          &
!                             flst,vlst,slst,ifld,ivct,iscl,     &
!                             nmax,                              &
!                             iftp,wfld,wvct,wscl,wgrd)
!   Write All data (function name, grid points, arrays) for UnstructureGrid
!
!   (IN)  cnme            [character(100)] : read file name
!   (IN)  iop             [integer]        : file unit number for IO
!   (IN)  flst(:)         [character(30)]  : name list for field  function
!   (IN)  vlst(:)         [character(30)]  : name list for vector function
!   (IN)  slst(:)         [character(30)]  : name list for scalar function
!   (IN)  ifld            [integer]        : number of field  functions
!   (IN)  ivct            [integer]        : number of vector functions
!   (IN)  iscl            [integer]        : number of scalar functions
!   (IN)  nmax            [integer]        : grid point
!   (IN)  iftp(:)         [integer]        : number of tuples for field array
!   (IN)  wfld(:)         [real(4)]        : field data
!   (IN)  wvct(:,:,:)     [real(4)]        : vector date
!   (IN)  wscl(:,:)       [real(4)]        : scalar data
!   (IN)  wgrd(:,:)       [real(4)]        : grid data
!
! VTK_ImagaData_Writer(cnme,iop,                                 &
!                      flst,vlst,slst,ifld,ivct,iscl,            &
!                      nmax,                                     &
!                      iftp,wfld,wvct,wscl,worn,wspc)
!   Write All data for Uniform Rectangular StructuredGrid
!
!   (IN)  cnme            [character(100)] : read file name
!   (IN)  iop             [integer]        : file unit number for IO
!   (IN)  flst(:)         [character(30)]  : name list for field  function
!   (IN)  vlst(:)         [character(30)]  : name list for vector function
!   (IN)  slst(:)         [character(30)]  : name list for scalar function
!   (IN)  ifld            [integer]        : number of field  functions
!   (IN)  ivct            [integer]        : number of vector functions
!   (IN)  iscl            [integer]        : number of scalar functions
!   (IN)  bgn,end         [integer]        : grid point
!   (IN)  iftp(:)         [integer]        : number of tuples for field array
!   (IN)  wfld(:)         [real(4)]        : field data
!   (IN)  wvct(:,:,:)     [real(4)]        : vector date
!   (IN)  wscl(:,:)       [real(4)]        : scalar data
!   (IN)  worn(1:3)       [real(4)]        : origin of grid point, x,y,z
!   (IN)  wspc(1:3)       [real(4)]        : spacing of grid point, x,y,z
!=================================================

contains

!=================================================


  subroutine VTK_Reader_GetNumberOfExtent(cnme,iop,iow,                       &
  &                                       jbgn,jend,kbgn,kend,lbgn,lend)
!-------------------------------------------------

  implicit none

  character(100)   , intent(in)  :: cnme
  integer          , intent(in)  :: iop, iow

  integer          , intent(out) :: jbgn
  integer, optional, intent(out) :: jend, kbgn, kend, lbgn, lend

  character(lth) :: cdata
  character(4)   :: cext
  integer        :: i1, i2

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

! check optional augument
  select case (cext(1:4))
  case ('vts ', 'vti ', 'pvts', 'pvti')

    if ( present(jend) ) then
    else
      write(iow,*) 'error optional augument is invalid.'
      write(iow,*) 'VTK XML StructuredGrid or VTK XML ImageData needs "bgn,end".'
      stop
    end if

  case ('vtu')

    if ( present(jend) ) then
      write(iow,*) 'error optional augument is invalid.'
      write(iow,*) 'VTK XML UnstructuredGrid only needs "number of pieces".'
      stop
    end if

  case ('pvtu')

    write(iow,*) 'error'
    write(iow,*) 'VTK XML PUnstructuredGrid have no data for number of extent.'
    write(iow,*) 'Please summantion procedure for piece files.'
    stop

  case default

    write(iow,*) 'error, unknown file name extention is selected.'
    stop

  end select

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read data before appended data section
  read(iop,end=999) cdata
999 continue

! total number of pieces
  select case (cext(1:4))
  case ('vts ', 'vti ')

    i1 =      index(cdata     ,'<Piece Extent="') + 15 ! start
    i2 = i1 + index(cdata(i1:),'">'             ) - 2  ! end
    read(cdata(i1:i2),*) jbgn, jend, kbgn, kend, lbgn, lend

  case ('vtu ')

    i1 = index(cdata,'<Piece NumberOfPoints="') + 23
    i2 = i1 + index(cdata(i1:),'"') - 2
    read(cdata(i1:i2),*) jbgn

  case ('pvts')

    i1 =      index(cdata     ,'<PStructuredGrid WholeExtent="') + 30 ! start
    i2 = i1 + index(cdata(i1:),'"'                             ) - 2  ! end
    read(cdata(i1:i2),*) jbgn, jend, kbgn, kend, lbgn, lend

  case ('pvti')

    i1 =      index(cdata     ,'<PImageData WholeExtent="') + 25 ! start
    i2 = i1 + index(cdata(i1:),'"'                        ) - 2  ! end
    read(cdata(i1:i2),*) jbgn, jend, kbgn, kend, lbgn, lend

  end select

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetNumberOfExtent

!=================================================

  subroutine VTK_Reader_GetNumberOfFunctions(cnme,iop,iow,                    &
  &                                          ifld,ivct,iscl)
!-------------------------------------------------

  implicit none

  character(100)   , intent(in)  :: cnme
  integer          , intent(in)  :: iop, iow

  integer          , intent(out) :: ifld, ivct, iscl

  character(lth) :: cdata
  character(4)   :: cext
  character(1)   :: cp
  integer        :: i1, i2, i3, i4, ipst, iped

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

  select case (cext(1:4))
  case ('vts ', 'vti ', 'vtu '); cp(1:1) = ' '
  case ('pvts', 'pvtu', 'pvti'); cp(1:1) = 'P'
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read data before appended data section
  read(iop,end=999) cdata
999 continue

! get number of functions

  !Field data

  ipst = index(cdata,'<' //trim(cp)//'FieldData>') ! start of point data section
  iped = index(cdata,'</'//trim(cp)//'FieldData>') ! end of point data section

  !initialize
  i1   = ipst + 12
  ifld = 0

  do

    !check existence of data array
    i2 = index(cdata(i1:iped),'<'//trim(cp)//'DataArray')
    if(i2==0) exit

    !end of data array
    i3 = i2 + index(cdata(i1+i2:iped),'/>')

    !count
    ifld = ifld + 1

    !update beginning position
    i1 = i1 + i3 + 2

  end do

  !Point Data

  ipst = index(cdata,'<' //trim(cp)//'PointData>') ! start of point data section
  iped = index(cdata,'</'//trim(cp)//'PointData>') ! end of point data section

  !initialize
  i1   = ipst + 12
  ivct = 0
  iscl = 0

  do

    !check existence of data array
    i2 = index(cdata(i1:iped),'<'//trim(cp)//'DataArray')
    if(i2==0) exit

    !end of data array
    i3 = i2 + index(cdata(i1+i2:iped),'/>')

    !check vector or scalar
    i4 = index(cdata(i1+i2:i1+i3),'NumberOfComponents')

    if(i4==0) then
      iscl = iscl + 1 ! scalar count
    else
      ivct = ivct + 1 ! vector count
    end if

    !update beginning position
    i1 = i1 + i3 + 2

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetNumberOfFunctions

!=================================================

  subroutine VTK_Reader_GetFunctionNames(cnme,iop,iow,                        &
  &                                      ifld,ivct,iscl,flst,vlst,slst)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  integer       , intent(in)  :: ifld, ivct, iscl

  character(30) , intent(out) :: flst(:), vlst(:), slst(:)

  character(lth) :: cdata
  character(4)   :: cext
  character(1)   :: cp
  integer        :: i, i1, i2, i3, i4, i5, i6, ipst, iped,                    &
  &                 ist, ivt, ip

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

  select case (cext(1:4))
  case ('vts ', 'vti ', 'vtu ')
    cp(1:1) = ' '
    ip = 0
  case ('pvts', 'pvtu', 'pvti')
    cp(1:1) = 'P'
    ip = 1
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read data before appended data section
  read(iop,end=999) cdata
999 continue

! get function names

  !initialize
  ist = 0
  ivt = 0

  !FieldData
  ipst = index(cdata,'<FieldData>')  ! start of point data section
  iped = index(cdata,'</FieldData>') ! end of point data section
  i1   = index(cdata,'<FieldData>') + 12

  do i = 1, ifld

    !position of target line start and end
    i2 = index(cdata(i1   :iped),'<DataArray') - 1
    i3 = index(cdata(i1+i2:iped),'/>'        ) + i2 + 1

    !get function name
    i4 = index(cdata(i1+i2:i1+i3),'Name="'   ) + i2 + 5
    i5 = index(cdata(i1+i4:i1+i3),'"'        ) + i4 - 2

    !get function name
    flst(i) = cdata(i1+i4:i1+i5)

    !update behining position
    i1 = i1 + i3 + 1

  end do

  !PointData
  ipst = index(cdata,'<' //trim(cp)//'PointData>') ! start of point data section
  iped = index(cdata,'</'//trim(cp)//'PointData>') ! end of point data section
  i1   = index(cdata,'<' //trim(cp)//'PointData>') + 12

  do i = 1, ivct+iscl

    !position of target line start and end
    i2 = index(cdata(i1   :iped),'<'//trim(cp)//'DataArray') - 1 + ip
    i3 = index(cdata(i1+i2:iped),'/>'                      ) + i2 + 1

    !get function name
    i4 = index(cdata(i1+i2:i1+i3),'Name="') + i2 + 5
    i5 = index(cdata(i1+i4:i1+i3),'"'     ) + i4 - 2

    !check vector or scalar
    i6 = index(cdata(i1+i2:i1+i3),'NumberOfComponents')

    if(i6==0) then
      ist = ist + 1
      slst(ist) = cdata(i1+i4:i1+i5)
    else
      ivt = ivt + 1
      vlst(ivt) = cdata(i1+i4:i1+i5)
    end if

    !update behining position
    i1 = i1 + i3 + 1

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetFunctionNames

!=================================================

  subroutine VTK_Reader_GetNumberOfTuples(cnme,iop,iow,                       &
  &                                       ifld,iftp)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  integer       , intent(in)  :: ifld

  integer       , intent(out) :: iftp(:)

  character(lth) :: cdata
  character(4)   :: cext
  character(1)   :: cp
  integer        :: i, i1, i2, i3, i4, i5, ipst, iped,                        &
  &                 ip

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

  select case (cext(1:4))
  case ('vts ', 'vti ', 'vtu ')
    cp(1:1) = ' '
    ip = 0
  case ('pvts', 'pvtu', 'pvti')
    cp(1:1) = 'P'
    ip = 1
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read data before appended data section
  read(iop,end=999) cdata
999 continue

! get number of tuples

  !FieldData
  ipst = index(cdata,'<FieldData>')  ! start of point data section
  iped = index(cdata,'</FieldData>') ! end of point data section
  i1 = ipst + 12

  do i = 1, ifld

    !position of target line start and end
    i2 = index(cdata(i1   :iped),'<DataArray') - 1
    i3 = index(cdata(i1+i2:iped),'/>'        ) + i2 + 1

    !get number of tuples
    i4 = index(cdata(i1+i2:i1+i3),'NumberOfTuples="') + i2 + 15
    i5 = index(cdata(i1+i4:i1+i3),'"'               ) + i4 - 2
    read(cdata(i1+i4:i1+i5),*) iftp(i)

    !update behining position
    i1 = i1 + i3 + 1

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetNumberOfTuples

!=================================================

  subroutine VTK_Reader_GetNumberOfPieces(cnme,iop,iow,                       &
  &                                       nprocs)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow

  integer       , intent(out) :: nprocs

  character(lth) :: cdata
  character(4)   :: cext
  integer        :: i1, i2, i3, ipst, iped

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! initialize
  select case (cext(1:4))
  case ('pvts')
    ipst = index(cdata,'</PPoints>'        )
    iped = index(cdata,'</PStructuredGrid>')
  case ('pvti')
    ipst = index(cdata,'</PPoints>'   )
    iped = index(cdata,'</PImageData>')
  case ('pvtu')
    ipst = index(cdata,'</PCells>'           )
    iped = index(cdata,'</PUnstructuredGrid>')
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

  i1     = ipst
  nprocs = 0

  do

    !check existence of data array
    i2 = index(cdata(i1:iped),'<Piece')
    if(i2==0) exit

    !end of data array
    i3 = i2 + index(cdata(i1+i2:iped),'/>')

    !count
    nprocs = nprocs + 1

    !update beginning position
    i1 = i1 + i3 + 2

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetNumberOfPieces

!=================================================

  subroutine VTK_Reader_GetPieceNames(cnme,iop,iow,nprocs,                    &
  &                                   cflt)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  integer       , intent(in)  :: nprocs

  character(100), intent(out) :: cflt(rs:)

  character(lth) :: cdata
  character(4)   :: cext
  integer        :: i, i1, i2, i3, i4, i5, ipst, iped

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! initialize
  select case (cext(1:4))
  case ('pvts', 'pvti')
    ipst = index(cdata,'</PPoints>'          )
    iped = index(cdata,'</PStructuredGrid>'  )
  case ('pvtu')
    ipst = index(cdata,'</PCells>'           )
    iped = index(cdata,'</PUnstructuredGrid>')
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

  i1 = ipst

  do i = 0, nprocs-1

    !position of target line start and end
    i2 = index(cdata(i1   :iped),'<Piece') - 1
    i3 = index(cdata(i1+i2:iped),'/>'    ) + i2 + 1

    !position of source
    i4 = index(cdata(i1+i2:i1+i3),'Source="') + i2 + 7
    i5 = index(cdata(i1+i4:i1+i3),'"'       ) + i4 - 2

    !get name
    cflt(i) = cdata(i1+i4:i1+i5)

    !update begining position
    i1 = i1 + i3 + 1

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetPieceNames

!=================================================

  subroutine VTK_Reader_GetPieceExtent_PStructuredGrid(cnme,iop,iow,nprocs,   &
  &                                                    nmax)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  integer       , intent(in)  :: nprocs

  integer       , intent(out) :: nmax(:,rs:)

  character(lth) :: cdata
  character(4)   :: cext
  integer        :: i, i1, i2, i3, i4, i5, ipst, iped

!-------------------------------------------------

! get file name extention
  i1 = index(trim(cnme),".",back=.true.) + 1
  cext = trim(cnme(i1:))

! file open
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! initialize
  select case (cext(1:4))
  case ('pvts', 'pvti')
    ipst = index(cdata,'</PPoints>'          )
    iped = index(cdata,'</PStructuredGrid>'  )
  case default
    write(iow,*) 'error, unknown file name extention is selected.'
    stop
  end select

  i1 = ipst

  do i = 0, nprocs-1

    !position of target line start and end
    i2 = index(cdata(i1   :iped),'<Piece') - 1
    i3 = index(cdata(i1+i2:iped),'/>'    ) + i2 + 1

    !position of extent
    i4 = index(cdata(i1+i2:i1+i3),'Extent="') + i2 + 7
    i5 = index(cdata(i1+i4:i1+i3),'"'       ) + i4 - 2

    read(cdata(i4:i5),*) nmax(1:6,i)

    !update begining position
    i1 = i1 + i3 + 1

  end do

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_GetPieceExtent_PStructuredGrid

!=================================================

  subroutine VTK_Reader_GetPieceExtent_PUnstructuredGrid(iop,nprocs,          &
  &                                                      cflt,nmax)
!-------------------------------------------------

  implicit none

  integer       , intent(in)  :: iop
  integer       , intent(in)  :: nprocs
  character(100), intent(in)  :: cflt(rs:)

  integer       , intent(out) :: nmax(rs:)

  character(lth) :: cdata
  integer        :: i, i1, i2, i3, i4

!-------------------------------------------------

  do i = 0, nprocs-1

# ifdef sxf90
    open(iop,file=trim(cflt(i)),form='unformatted',status='old')
# else
    open(iop,file=trim(cflt(i)),form='unformatted',status='old',access='stream')
# endif

    read(iop,end=998) cdata
998 continue

    i1 = index(cdata     ,'<Piece') - 1
    i2 = index(cdata(i1:),'/>'    ) + i1 + 1

    i3 = index(cdata(i1   :i2),'NumberOfPoints="') + 16
    i4 = index(cdata(i1+i3:i2),'"'               ) + i3 - 2
    read(cdata(i1+i3:i1+i4),*) nmax(i)

    close(iop)

  end do

!-------------------------------------------------
  end subroutine VTK_Reader_GetPieceExtent_PUnstructuredGrid

!=================================================

  subroutine VTK_Reader_StructuredGrid_Single(cnme,iop,iow,cdbg,              &
  &                                           flst,vlst,slst,ifld,ivct,iscl,  &
  &                                           jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                           iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  character(4)  , intent(in)  :: cdbg

  character(30) , intent(in)  :: flst(:), vlst(:), slst(:)
  integer       , intent(in)  :: ifld, ivct, iscl,                            &
  &                              jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in)  :: iftp(:)
  real(4)       , intent(out) :: wfld(:),                                     &
  &                              wvct(:,jbgn:,kbgn:,lbgn:,:),                 &
  &                              wscl(  jbgn:,kbgn:,lbgn:,:),                 &
  &                              wgrd(:,jbgn:,kbgn:,lbgn:)

  character(30) , allocatable :: flst_now(:), vlst_now(:), slst_now(:)
  integer       , allocatable :: iftp_now(:)
  integer :: ifld_now, ivct_now, iscl_now,                                    &
  &          jbgn_now, jend_now, kbgn_now, kend_now, lbgn_now, lend_now

  character(lth) :: cdata
  integer        :: iapp, i, i1, ist, ied

!-------------------------------------------------

! first open to check function and position of appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! get position of appended data section
  iapp = index(cdata,'<AppendedData encoding="raw">') + 31
  if (iapp == 31) then
    write(*,*) 'length of character function "chara" is too short.'
    write(*,*) 'beginning position of appended data section could not found.'
    stop
  end if

  close(iop)

! check array names
  if (cdbg(1:4) == 'enbl') then

  ! read number of pieces
    call VTK_Reader_GetNumberOfExtent(cnme,iop,iow,                           &
    &                                 jbgn_now,jend_now,                      &
    &                                 kbgn_now,kend_now,                      &
    &                                 lbgn_now,lend_now)

  ! read number of functions
    call VTK_Reader_GetNumberOfFunctions(cnme,iop,iow,ifld_now,ivct_now,iscl_now)

    !check array size and number of functions
    if ( jend-jbgn/=jend_now-jbgn_now .or. kend-kbgn/=kend_now-kbgn_now       &
    &.or.lend-lbgn/=lend_now-lbgn_now ) then
      write(iow,*) 'Error, grid size is wrong.'
      write(iow,*) 'READ FILE :',jend    -jbgn    +1, kend    -kbgn    +1,    &
      &                          lend    -lbgn    +1
      write(iow,*) 'ARGUMENT  :',jend_now-jbgn_now+1, kend_now-kbgn_now+1,    &
      &                          lend_now-lbgn_now+1
      stop
    end if

    if (ifld /= ifld_now) then
      write(iow,*) 'Error, number of field function is wrong.'
      write(iow,*) 'READ FILE :',ifld_now
      write(iow,*) 'ARGUMENT  :',ifld
      stop
    end if

    if (ivct /= ivct_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',ivct_now
      write(iow,*) 'ARGUMENT  :',ivct
      stop
    end if

    if (iscl /= iscl_now) then
      write(iow,*) 'Error, number of scalor function is wrong.'
      write(iow,*) 'READ FILE :',iscl_now
      write(iow,*) 'ARGUMENT  :',iscl
      stop
    end if

  ! read function name and number of tuples
    allocate(iftp_now(ifld_now),                                              &
    &        flst_now(ifld_now),vlst_now(ivct_now),slst_now(iscl_now))

    call VTK_Reader_GetFunctionNames(cnme,iop,iow,ifld,ivct,iscl,             &
    &                                flst_now,vlst_now,slst_now)

    call VTK_Reader_GetNumberOfTuples(cnme,iop,iow,ifld,iftp_now)

    !check name of functions
    do i = 1, ifld_now
      if (flst(i) /= flst_now(i) .or. iftp(i) /= iftp_now(i)) then
        write(iow,*) 'Error, name of field function is wrong.'
        write(iow,*) 'READ FILE :',flst_now(:)
        write(iow,*) 'TUPLES    :',iftp_now(:)
        write(iow,*) 'ARGUMENT  :',flst(:)
        write(iow,*) 'TUPLES    :',iftp(:)
        stop
      end if
    end do

    do i = 1, ivct_now
      if (vlst(i) /= vlst_now(i)) then
        write(iow,*) 'Error, name of vector function is wrong.'
        write(iow,*) 'READ FILE :',vlst_now(:)
        write(iow,*) 'ARGUMENT  :',vlst(:)
        stop
      end if
    end do

    do i = 1, iscl_now
      if (slst(i) /= slst_now(i)) then
        write(iow,*) 'Error, name of vector function is wrong.'
        write(iow,*) 'READ FILE :',slst_now(:)
        write(iow,*) 'ARGUMENT  :',slst(:)
        stop
      end if
    end do

    deallocate(iftp_now,flst_now,vlst_now,slst_now)

  end if

! second open to get appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! move appended data section
  read(iop) cdata(:iapp)

! get data
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    read(iop) i1, wfld(ist:ied)
  end do
  do i = 1, ivct
    read(iop) i1, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    read(iop) i1, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  read(iop) i1, wgrd(1:3,jbgn:jend,kbgn:kend,lbgn:lend)

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_StructuredGrid_Single

!=================================================

  subroutine VTK_Reader_StructuredGrid_Double(cnme,iop,iow,cdbg,              &
  &                                           flst,vlst,slst,ifld,ivct,iscl,  &
  &                                           jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                           iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  character(4)  , intent(in)  :: cdbg

  character(30) , intent(in)  :: flst(:), vlst(:), slst(:)
  integer       , intent(in)  :: ifld, ivct, iscl,                            &
  &                              jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in)  :: iftp(:)
  real(8)       , intent(out) :: wfld(:),                                     &
  &                              wvct(:,jbgn:,kbgn:,lbgn:,:),                 &
  &                              wscl(  jbgn:,kbgn:,lbgn:,:),                 &
  &                              wgrd(:,jbgn:,kbgn:,lbgn:)

  integer       , allocatable :: iftp_now(:)
  character(30) , allocatable :: flst_now(:), vlst_now(:), slst_now(:)
  integer :: ifld_now, ivct_now, iscl_now,                                    &
  &          jbgn_now, jend_now, kbgn_now, kend_now, lbgn_now, lend_now

  character(lth) :: cdata
  integer        :: iapp, i, i1, ist, ied

!-------------------------------------------------

! first open to check function and position of appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! get position of appended data section
  iapp = index(cdata,'<AppendedData encoding="raw">') + 31
  if (iapp == 31) then
    write(*,*) 'length of character function "chara" is too short.'
    write(*,*) 'beginning position of appended data section could not found.'
    stop
  end if

  close(iop)

! check array names
  if (cdbg(1:4) == 'enbl') then

  ! read number of pieces
    call VTK_Reader_GetNumberOfExtent(cnme,iop,iow,                           &
    &                                 jbgn_now,jend_now,                      &
    &                                 kbgn_now,kend_now,                      &
    &                                 lbgn_now,lend_now)

  ! read number of functions
    call VTK_Reader_GetNumberOfFunctions(cnme,iop,iow,ifld_now,ivct_now,iscl_now)

    !check array size and number of functions
    if ( jend-jbgn/=jend_now-jbgn_now .or. kend-kbgn/=kend_now-kbgn_now       &
    &.or.lend-lbgn/=lend_now-lbgn_now ) then
      write(iow,*) 'Error, grid size is wrong.'
      write(iow,*) 'READ FILE :',jend    -jbgn    +1, kend    -kbgn    +1,    &
      &                          lend    -lbgn    +1
      write(iow,*) 'ARGUMENT  :',jend_now-jbgn_now+1, kend_now-kbgn_now+1,    &
      &                          lend_now-lbgn_now+1
      stop
    end if

    if (ifld /= ifld_now) then
      write(iow,*) 'Error, number of field function is wrong.'
      write(iow,*) 'READ FILE :',ifld_now
      write(iow,*) 'ARGUMENT  :',ifld
      stop
    end if

    if (ivct /= ivct_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',ivct_now
      write(iow,*) 'ARGUMENT  :',ivct
      stop
    end if

    if (iscl /= iscl_now) then
      write(iow,*) 'Error, number of scalor function is wrong.'
      write(iow,*) 'READ FILE :',iscl_now
      write(iow,*) 'ARGUMENT  :',iscl
      stop
    end if

  ! read function name and number of tuples
    allocate(iftp_now(ifld_now),                                              &
    &        flst_now(ifld_now),vlst_now(ivct_now),slst_now(iscl_now))

    call VTK_Reader_GetFunctionNames(cnme,iop,iow,ifld,ivct,iscl,             &
    &                                flst_now,vlst_now,slst_now)

    call VTK_Reader_GetNumberOfTuples(cnme,iop,iow,ifld,iftp_now)

    !check name of functions
    do i = 1, ifld_now
      if (flst(i) /= flst_now(i) .or. iftp(i) /= iftp_now(i)) then
        write(iow,*) 'Error, name of field function is wrong.'
        write(iow,*) 'READ FILE :',flst_now(:)
        write(iow,*) 'TUPLES    :',iftp_now(:)
        write(iow,*) 'ARGUMENT  :',flst(:)
        write(iow,*) 'TUPLES    :',iftp(:)
        stop
      end if
    end do

    do i = 1, ivct_now
      if (vlst(i) /= vlst_now(i)) then
        write(iow,*) 'Error, name of vector function is wrong.'
        write(iow,*) 'READ FILE :',vlst_now(:)
        write(iow,*) 'ARGUMENT  :',vlst(:)
        stop
      end if
    end do

    do i = 1, iscl_now
      if (slst(i) /= slst_now(i)) then
        write(iow,*) 'Error, name of vector function is wrong.'
        write(iow,*) 'READ FILE :',slst_now(:)
        write(iow,*) 'ARGUMENT  :',slst(:)
        stop
      end if
    end do

    deallocate(iftp_now,flst_now,vlst_now,slst_now)

  end if

! second open to get appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! move appended data section
  read(iop) cdata(:iapp)

! get data
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    read(iop) i1, wfld(ist:ied)
  end do
  do i = 1, ivct
    read(iop) i1, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    read(iop) i1, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  read(iop) i1, wgrd(1:3,jbgn:jend,kbgn:kend,lbgn:lend)

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_StructuredGrid_Double

!=================================================

  subroutine VTK_Reader_UnstructuredGrid_Single(cnme,iop,iow,cdbg,            &
  &                                             flst,vlst,slst,ifld,ivct,iscl,&
  &                                             nmax,                         &
  &                                             iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  character(4)  , intent(in)  :: cdbg

  character(30) , intent(in)  :: flst(:), vlst(:), slst(:)
  integer       , intent(in)  :: ifld, ivct, iscl, nmax
  integer       , intent(in)  :: iftp(:)
  real(4)       , intent(out) :: wfld(:), wvct(:,:,:), wscl(:,:), wgrd(:,:)

  integer       , allocatable :: iftp_now(:)
  character(30) , allocatable :: flst_now(:), vlst_now(:), slst_now(:)
  integer :: ifld_now, ivct_now, iscl_now, nmax_now

  character(lth) :: cdata
  integer :: iapp, i, i1, ist, ied

!-------------------------------------------------

! first open to check function and position of appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! get position of appended data section
  iapp = index(cdata,'<AppendedData encoding="raw">') + 31
  if (iapp == 31) then
    write(*,*) 'length of character function "chara" is too short.'
    write(*,*) 'beginning position of appended data section could not found.'
    stop
  end if

  close(iop)

! check array names
  if (cdbg(1:4)=='enbl') then

  ! read number of pieces
    call VTK_Reader_GetNumberOfExtent(cnme,iop,iow,nmax_now)

  ! read number of functions
    call VTK_Reader_GetNumberOfFunctions(cnme,iop,iow,ifld_now,ivct_now,iscl_now)

    !check array size and number of functions
    if (nmax /= nmax_now) then
      write(iow,*) 'Error, grid size is wrong.'
      write(iow,*) 'READ FILE :',nmax_now
      write(iow,*) 'ARGUMENT  :',nmax
      stop
    end if

    if (ifld /= ifld_now) then
      write(iow,*) 'Error, number of field function is wrong.'
      write(iow,*) 'READ FILE :',ifld_now
      write(iow,*) 'ARGUMENT  :',ifld
      stop
    end if

    if (ivct /= ivct_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',ivct_now
      write(iow,*) 'ARGUMENT  :',ivct
      stop
    end if

    if (iscl /= iscl_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',iscl_now
      write(iow,*) 'ARGUMENT  :',iscl
      stop
    end if

  ! read function name and number of tuples
    allocate(iftp_now(ifld_now),                                              &
    &        flst_now(ifld_now),vlst_now(ivct_now),slst_now(iscl_now))

    call VTK_Reader_GetFunctionNames(cnme,iop,iow,ifld,ivct,iscl,             &
    &                                flst_now,vlst_now,slst_now)

    call VTK_Reader_GetNumberOfTuples(cnme,iop,iow,ifld,iftp_now)

    do i = 1, ifld_now
      if (flst(i) /= flst_now(i) .or. iftp(i) /= iftp_now(i)) then
        write(iow,*) 'Error, name of field function is wrong.'
        write(iow,*) 'READ FILE :',flst_now(:)
        write(iow,*) 'TUPLES    :',iftp_now(:)
        write(iow,*) 'ARGUMENT  :',flst(:)
        write(iow,*) 'TUPLES    :',iftp(:)
        stop
      end if
    end do

    do i = 1, ivct_now
      if (vlst(i) /= vlst_now(i)) then
        write(iow,*) 'Error, number of vector function is wrong.'
        write(iow,*) 'READ FILE :',vlst_now(:)
        write(iow,*) 'ARGUMENT  :',vlst(:)
        stop
      end if
    end do

    do i = 1, iscl_now
      if (slst(i) /= slst_now(i)) then
        write(iow,*) 'Error, number of vector function is wrong.'
        write(iow,*) 'READ FILE :',slst_now(:)
        write(iow,*) 'ARGUMENT  :',slst(:)
        stop
      end if
    end do

    deallocate(iftp_now,flst_now,vlst_now,slst_now)

  end if

! second open to get appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! move appended data section
  read(iop) cdata(:iapp)

! get data
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    read(iop) i1, wfld(ist:ied)
  end do
  do i = 1, ivct
    read(iop) i1, wvct(1:3,1:nmax,i)
  end do
  do i = 1, iscl
    read(iop) i1, wscl(1:nmax,i)
  end do
  read(iop) i1, wgrd(1:3,1:nmax)

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_UnstructuredGrid_Single

!=================================================

  subroutine VTK_Reader_UnstructuredGrid_Double(cnme,iop,iow,cdbg,            &
  &                                             flst,vlst,slst,ifld,ivct,iscl,&
  &                                             nmax,                         &
  &                                             iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none

  character(100), intent(in)  :: cnme
  integer       , intent(in)  :: iop, iow
  character(4)  , intent(in)  :: cdbg

  character(30) , intent(in)  :: flst(:), vlst(:), slst(:)
  integer       , intent(in)  :: ifld, ivct, iscl, nmax
  integer       , intent(in)  :: iftp(:)
  real(8)       , intent(out) :: wfld(:), wvct(:,:,:), wscl(:,:), wgrd(:,:)

  integer       , allocatable :: iftp_now(:)
  character(30) , allocatable :: flst_now(:), vlst_now(:), slst_now(:)
  integer :: ifld_now, ivct_now, iscl_now, nmax_now

  character(lth) :: cdata
  integer :: iapp, i, i1, ist, ied

!-------------------------------------------------

! first open to check function and position of appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! read character date before appended data section
  read(iop,end=999) cdata
999 continue

! get position of appended data section
  iapp = index(cdata,'<AppendedData encoding="raw">') + 31
  if (iapp == 31) then
    write(*,*) 'length of character function "chara" is too short.'
    write(*,*) 'beginning position of appended data section could not found.'
    stop
  end if

  close(iop)

! check array names
  if (cdbg(1:4)=='enbl') then

  ! read number of pieces
    call VTK_Reader_GetNumberOfExtent(cnme,iop,iow,nmax_now)

  ! read number of functions
    call VTK_Reader_GetNumberOfFunctions(cnme,iop,iow,ifld_now,ivct_now,iscl_now)

    if (nmax /= nmax_now) then
      write(iow,*) 'Error, grid size is wrong.'
      write(iow,*) 'READ FILE :',nmax_now
      write(iow,*) 'ARGUMENT  :',nmax
      stop
    end if

    if (ifld /= ifld_now) then
      write(iow,*) 'Error, number of field function is wrong.'
      write(iow,*) 'READ FILE :',ifld_now
      write(iow,*) 'ARGUMENT  :',ifld
      stop
    end if

    if (ivct /= ivct_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',ivct_now
      write(iow,*) 'ARGUMENT  :',ivct
      stop
    end if

    if (iscl /= iscl_now) then
      write(iow,*) 'Error, number of vector function is wrong.'
      write(iow,*) 'READ FILE :',iscl_now
      write(iow,*) 'ARGUMENT  :',iscl
      stop
    end if

  ! read function name and number of tuples
    allocate(iftp_now(ifld_now),                                              &
    &        flst_now(ifld_now),vlst_now(ivct_now),slst_now(iscl_now))

    call VTK_Reader_GetFunctionNames(cnme,iop,iow,ifld,ivct,iscl,             &
    &                                flst_now,vlst_now,slst_now)

    call VTK_Reader_GetNumberOfTuples(cnme,iop,iow,ifld,iftp_now)

    do i = 1, ifld_now
      if (flst(i) /= flst_now(i) .or. iftp(i) /= iftp_now(i)) then
        write(iow,*) 'Error, name of field function is wrong.'
        write(iow,*) 'READ FILE :',flst_now(:)
        write(iow,*) 'TUPLES    :',iftp_now(:)
        write(iow,*) 'ARGUMENT  :',flst(:)
        write(iow,*) 'TUPLES    :',iftp(:)
        stop
      end if
    end do

    do i = 1, ivct_now
      if (vlst(i) /= vlst_now(i)) then
        write(iow,*) 'Error, number of vector function is wrong.'
        write(iow,*) 'READ FILE :',vlst_now(:)
        write(iow,*) 'ARGUMENT  :',vlst(:)
        stop
      end if
    end do

    do i = 1, iscl_now
      if (slst(i) /= slst_now(i)) then
        write(iow,*) 'Error, number of vector function is wrong.'
        write(iow,*) 'READ FILE :',slst_now(:)
        write(iow,*) 'ARGUMENT  :',slst(:)
        stop
      end if
    end do

    deallocate(iftp_now,flst_now,vlst_now,slst_now)

  end if

! second open to get appended data
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='old')
#else
  open(iop,file=trim(cnme),form='unformatted',status='old',access='stream')
#endif

! move appended data section
  read(iop) cdata(:iapp)

! get data
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    read(iop) i1, wfld(ist:ied)
  end do
  do i = 1, ivct
    read(iop) i1, wvct(1:3,1:nmax,i)
  end do
  do i = 1, iscl
    read(iop) i1, wscl(1:nmax,i)
  end do
  read(iop) i1, wgrd(1:3,1:nmax)

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Reader_UnstructuredGrid_Double

!=================================================

  subroutine VTK_Writer_StructuredGrid_Single(cnme,cpnt,iop,                  &
  &                                           flst,vlst,slst,ifld,ivct,iscl,  &
  &                                           jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                           iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  character(50) , intent(in) :: cpnt
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl,                             &
  &                             jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in) :: iftp(:)
  real(4)       , intent(in) :: wfld(:),                                      &
  &                             wvct(:,jbgn:,kbgn:,lbgn:,:),                  &
  &                             wscl(  jbgn:,kbgn:,lbgn:,:),                  &
  &                             wgrd(:,jbgn:,kbgn:,lbgn:)

  character(30) :: coff, ctpl, coff1, coff2, coff3, coff4
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 4
  integer :: bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

! bytes of functions
  bytes_scalar =   (jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo
  bytes_vector = 3*(jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="StructuredGrid" version="0.1"',                  &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<StructuredGrid WholeExtent="'//cpnt//'">', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff//'" />',eol

  end do

  !close field data section
  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece Extent="'//cpnt//'">', eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />', eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !coordinate data declaration
  write(iop) '<Points>', eol
  write(iop) '<DataArray type="'//ctype//'" NumberOfComponents="3"',          &
  &          ' format="appended" offset="'//coff4//'" />', eol
  write(iop) '</Points>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</StructuredGrid>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  write(iop) bytes_vector, wgrd(1:3,jbgn:jend,kbgn:kend,lbgn:lend)

  !close appended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_StructuredGrid_Single

!=================================================

  subroutine VTK_Writer_StructuredGrid_Double(cnme,cpnt,iop,                  &
  &                                           flst,vlst,slst,ifld,ivct,iscl,  &
  &                                           jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                           iftp,wfld,wvct,wscl,wgrd)
! Copied by VTK_Writer_StructuredGrid_Single
! Change Log : real(4)     , intent(in) :: wfld ...          => real(8), intent(in) ...
!              integer     , parameter  :: bytes_flo = 4     => bytes_flo = 8
!              character(7), parameter  :: ctype = 'Float32' => ctype = 'Float64'
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  character(50) , intent(in) :: cpnt
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl,                             &
  &                             jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in) :: iftp(:)
  real(8)       , intent(in) :: wfld(:),                                      &
  &                             wvct(:,jbgn:,kbgn:,lbgn:,:),                  &
  &                             wscl(  jbgn:,kbgn:,lbgn:,:),                  &
  &                             wgrd(:,jbgn:,kbgn:,lbgn:)

  character(30) :: coff, ctpl, coff1, coff2, coff3, coff4
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 8
  integer :: bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float64'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

! bytes of functions
  bytes_scalar =   (jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo
  bytes_vector = 3*(jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="StructuredGrid" version="0.1"',                  &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<StructuredGrid WholeExtent="'//cpnt//'">', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff//'" />',eol

  end do

  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece Extent="'//cpnt//'">', eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />', eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !coordinate data declaration
  write(iop) '<Points>', eol
  write(iop) '<DataArray type="'//ctype//'" NumberOfComponents="3"',          &
  &          ' format="appended" offset="'//coff4//'" />', eol
  write(iop) '</Points>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</StructuredGrid>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  write(iop) bytes_vector, wgrd(1:3,jbgn:jend,kbgn:kend,lbgn:lend)

  !close appended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_StructuredGrid_Double

!=================================================

  subroutine VTK_Writer_PStructuredGrid_Single(cnme,iop,nprocs,                &
  &                                            vlst,slst,ivct,iscl,            &
  &                                            bgns,ends,cn_lst)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  integer       , intent(in) :: iop
  integer       , intent(in) :: nprocs

  character(30) , intent(in) :: vlst(:), slst(:)
  integer       , intent(in) :: ivct, iscl
  integer       , intent(in) :: bgns(rs:,:), ends(rs:,:)
  character(100), intent(in) :: cn_lst(rs:)

  character(50) :: cpnt
  character(4)  :: cprc
  integer :: re, i

  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

  re = nprocs-1

  !whole region
  write(cpnt,'(3(i7,1x,i7,1x))') bgns(rs,1), ends(re,1),                      &
  &                              bgns(rs,2), ends(re,2),                      &
  &                              bgns(rs,3), ends(re,3)

#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="PStructuredGrid" version="0.1"',                 &
  &            ' byte_order="LittleEndian">', eol
  write(iop)'<PStructuredGrid WholeExtent="'//cpnt//'" GhostLevel="1">', eol

  !point data section
  write(iop) '<PPointData>', eol

  !vector data declaration
  do i = 1, ivct
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',   &
    &          ' NumberOfComponents="3" />', eol
  end do

  !scalar data declaration
  do i = 1, iscl
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(slst(i))//'" />', eol
  end do

  !close point data section
  write(iop) '</PPointData>', eol

  !coordinate data declaration
  write(iop) '<PPoints>', eol
  write(iop) '<PDataArray type="'//ctype//'" NumberOfComponents="3" />', eol
  write(iop) '</PPoints>', eol

  !table of indivisual pieces
  do i = rs, re
    write(cprc,'(i4.4)') i
    write(cpnt,'(3(i7,1x,i7,1x))') bgns(i,1), ends(i,1),                      &
    &                              bgns(i,2), ends(i,2),                      &
    &                              bgns(i,3), ends(i,3)
    write(iop) '<Piece Extent="'//cpnt//'" Source="'//trim(cn_lst(i))//'" />',&
    &          eol
  end do

  !close tags
  write(iop) '</PStructuredGrid>', eol
  write(iop) '</VTKFile>', eol

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_PStructuredGrid_Single

!=================================================

  subroutine VTK_Writer_UnstructuredGrid_Single(cnme,iop,                     &
  &                                             flst,vlst,slst,ifld,ivct,iscl,&
  &                                             nmax,                         &
  &                                             iftp,wfld,wvct,wscl,wgrd)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl, nmax
  integer       , intent(in) :: iftp(:)
  real(4)       , intent(in) :: wfld(:), wvct(:,:,:), wscl(:,:), wgrd(:,:)

  integer, allocatable :: cell_type(:), cell_offset(:)

  character(50) :: cpnt
  character(30) :: coff, ctpl, coff1, coff2, coff3, coff4, coff5, coff6, coff7
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, ioff5, ioff6, ioff7, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 4
  integer :: bytes_integer, bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

  allocate(cell_type(1:nmax),cell_offset(1:nmax))
  cell_type(1:nmax) = 1
  cell_offset(1:nmax) = 0

! bytes of functions
  bytes_integer=   nmax*bytes_int
  bytes_scalar =   nmax*bytes_flo
  bytes_vector = 3*nmax*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)
  ioff5 = ioff4 +       bytes_int + bytes_vector
  ioff6 = ioff5 +       bytes_int + bytes_integer
  ioff7 = ioff6 +       bytes_int + bytes_integer

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4
  write(coff5,'(i30)') ioff5
  write(coff6,'(i30)') ioff6
  write(coff7,'(i30)') ioff7

! total grid points
  write(cpnt,'(i12)') nmax

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="UnstructuredGrid" version="0.1"',                &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<UnstructuredGrid>', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff1//'" />',eol

  end do

  !close field data section
  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece NumberOfPoints="'//cpnt//'" NumberOfCells="'//cpnt//'">',&
  &          eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />',eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !coordinate data declaration
  write(iop) '<Points>', eol
  write(iop) '<DataArray type="'//ctype//'" NumberOfComponents="3"',          &
  &          ' format="appended" offset="'//coff4//'" />', eol
  write(iop) '</Points>', eol

  !unstructured grid attribute
  write(iop) '<Cells>', eol
  write(iop) '<DataArray type="Int8" Name="connectivity"',                    &
  &          ' format="appended" offset="'//coff5//'" />', eol
  write(iop) '<DataArray type="Int8" Name="offsets"',                         &
  &          ' format="appended" offset="'//coff6//'" />', eol
  write(iop) '<DataArray type="Int8" Name="types"',                           &
  &          ' format="appended" offset="'//coff7//'" />', eol
  write(iop) '</Cells>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</UnstructuredGrid>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,1:nmax,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(1:nmax,i)
  end do
  write(iop) bytes_vector, wgrd(1:3,1:nmax)
  write(iop) bytes_integer,cell_type(1:nmax)
  write(iop) bytes_integer,cell_offset(1:nmax)
  write(iop) bytes_integer,cell_type(1:nmax)

  !close appdended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_UnstructuredGrid_Single

!=================================================

  subroutine VTK_Writer_UnstructuredGrid_Double(cnme,iop,                     &
  &                                             flst,vlst,slst,ifld,ivct,iscl,&
  &                                             nmax,                         &
  &                                             iftp,wfld,wvct,wscl,wgrd)
! Copied by VTK_Writer_StructuredGrid_Single
! Change Log : real(4)     , intent(in) :: wfld ...          => real(8), intent(in) ...
!              integer     , parameter  :: bytes_flo = 4     => bytes_flo = 8
!              character(7), parameter  :: ctype = 'Float32' => ctype = 'Float64'
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl, nmax
  integer       , intent(in) :: iftp(:)
  real(8)       , intent(in) :: wfld(:), wvct(:,:,:), wscl(:,:), wgrd(:,:)

  integer, allocatable :: cell_type(:), cell_offset(:)

  character(50) :: cpnt
  character(30) :: coff, ctpl, coff1, coff2, coff3, coff4, coff5, coff6, coff7
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, ioff5, ioff6, ioff7, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 8
  integer :: bytes_integer, bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float64'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

  allocate(cell_type(1:nmax),cell_offset(1:nmax))
  cell_type(1:nmax) = 1
  cell_offset(1:nmax) = 0

! bytes of functions
  bytes_integer=   nmax*bytes_int
  bytes_scalar =   nmax*bytes_flo
  bytes_vector = 3*nmax*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)
  ioff5 = ioff4 +       bytes_int + bytes_vector
  ioff6 = ioff5 +       bytes_int + bytes_integer
  ioff7 = ioff6 +       bytes_int + bytes_integer

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4
  write(coff5,'(i30)') ioff5
  write(coff6,'(i30)') ioff6
  write(coff7,'(i30)') ioff7

! total grid points
  write(cpnt,'(i12)') nmax

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="UnstructuredGrid" version="0.1"',                &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<UnstructuredGrid>', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff1//'" />',eol

  end do

  !close field data section
  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece NumberOfPoints="'//cpnt//'" NumberOfCells="'//cpnt//'">',&
  &          eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />',eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !coordinate data declaration
  write(iop) '<Points>', eol
  write(iop) '<DataArray type="'//ctype//'" NumberOfComponents="3"',          &
  &          ' format="appended" offset="'//coff4//'" />', eol
  write(iop) '</Points>', eol

  !unstructured grid attribute
  write(iop) '<Cells>', eol
  write(iop) '<DataArray type="Int8" Name="connectivity"',                    &
  &          ' format="appended" offset="'//coff5//'" />', eol
  write(iop) '<DataArray type="Int8" Name="offsets"',                         &
  &          ' format="appended" offset="'//coff6//'" />', eol
  write(iop) '<DataArray type="Int8" Name="types"',                           &
  &          ' format="appended" offset="'//coff7//'" />', eol
  write(iop) '</Cells>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</UnstructuredGrid>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,1:nmax,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(1:nmax,i)
  end do
  write(iop) bytes_vector, wgrd(1:3,1:nmax)
  write(iop) bytes_integer,cell_type(1:nmax)
  write(iop) bytes_integer,cell_offset(1:nmax)
  write(iop) bytes_integer,cell_type(1:nmax)

  !close appdended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_UnstructuredGrid_Double

!=================================================

  subroutine VTK_Writer_PUnstructuredGrid_Single(cnme,iop,nprocs,             &
  &                                              vlst,slst,ivct,iscl,         &
  &                                              cn_lst)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  integer       , intent(in) :: iop
  integer       , intent(in) :: nprocs

  character(30) , intent(in) :: vlst(:), slst(:)
  integer       , intent(in) :: ivct, iscl
  character(100), intent(in) :: cn_lst(rs:)

  integer :: re, i

  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

  re = nprocs-1

#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="PUnstructuredGrid" version="0.1"',               &
  &          ' byte_order="LittleEndian">', eol
  write(iop)'<PUnstructuredGrid GhostLevel="0">', eol

  !point data section
  write(iop) '<PPointData>', eol

  !vector data declaration
  do i = 1, ivct
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',   &
    &          ' NumberOfComponents="3" />', eol
  end do

  !scalar data declaration
  do i = 1, iscl
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(slst(i))//'" />', eol
  end do

  !close point data section
  write(iop) '</PPointData>', eol

  !coordinate data declaration
  write(iop) '<PPoints>', eol
  write(iop) '<PDataArray type="'//ctype//'" NumberOfComponents="3" />', eol
  write(iop) '</PPoints>', eol

  !unstuructured grid attribute
  write(iop) '<PCells>', eol
  write(iop) '<PDataArray type="Int8" Name="connectivity" />', eol
  write(iop) '<PDataArray type="Int8" Name="offsets" />', eol
  write(iop) '<PDataArray type="Int8" Name="types" />', eol
  write(iop) '</PCells>', eol

  !table of indivisual pieces
  do i = rs, re
    write(iop) '<Piece Source="'//trim(cn_lst(i))//'" />', eol
  end do

  !close tags
  write(iop) '</PUnstructuredGrid>', eol
  write(iop) '</VTKFile>', eol

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_PUnstructuredGrid_Single

!=================================================

  subroutine VTK_Writer_ImageData_Single(cnme,cpnt,iop,                  &
  &                                      flst,vlst,slst,ifld,ivct,iscl,  &
  &                                      jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                      iftp,wfld,wvct,wscl,worn,wspc)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  character(50) , intent(in) :: cpnt
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl,                             &
  &                             jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in) :: iftp(:)
  real(4)       , intent(in) :: wfld(:),                                      &
  &                             wvct(:,jbgn:,kbgn:,lbgn:,:),                  &
  &                             wscl(  jbgn:,kbgn:,lbgn:,:),                  &
  &                             worn(:), wspc(:)

  character(30)  :: coff, ctpl, coff1, coff2, coff3, coff4
  character(100) :: corn, cspc
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 4
  integer :: bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

! bytes of functions
  bytes_scalar =   (jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo
  bytes_vector = 3*(jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4

! grid origin and spacing
  write(corn,'(3(e15.8,1x,e15.8,1x,e15.8))') worn(1:3)
  write(cspc,'(3(e15.8,1x,e15.8,1x,e15.8))') wspc(1:3)

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="ImageData" version="0.1"',                       &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<ImageData WholeExtent="'//cpnt//'" Origin="'//trim(corn)//'"', &
  &          ' Spacing="'//trim(cspc)//'">', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff//'" />',eol

  end do

  !close field data section
  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece Extent="'//cpnt//'">', eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />', eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</ImageData>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do

  !close appended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_ImageData_Single

!=================================================

  subroutine VTK_Writer_ImageData_Double(cnme,cpnt,iop,                  &
  &                                      flst,vlst,slst,ifld,ivct,iscl,  &
  &                                      jbgn,jend,kbgn,kend,lbgn,lend,  &
  &                                      iftp,wfld,wvct,wscl,worn,wspc)
! Copied by VTK_Writer_StructuredGrid_Single
! Change Log : real(4)     , intent(in) :: wfld ...          => real(8), intent(in) ...
!              integer     , parameter  :: bytes_flo = 4     => bytes_flo = 8
!              character(7), parameter  :: ctype = 'Float32' => ctype = 'Float64'
!
!              write(corn,'(3(e15.8,1x,e15.8,1x,e15.8))') worn(1:3)
!              write(cspc,'(3(e15.8,1x,e15.8,1x,e15.8))') wspc(1:3)
!           => write(corn,'(3(e23.16,1x,e23.16,1x,e23.16))') worn(1:3)
!              write(cspc,'(3(e23.16,1x,e23.16,1x,e23.16))') wspc(1:3)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  character(50) , intent(in) :: cpnt
  integer       , intent(in) :: iop

  character(30) , intent(in) :: flst(:), vlst(:), slst(:)
  integer       , intent(in) :: ifld, ivct, iscl,                             &
  &                             jbgn, jend, kbgn, kend, lbgn, lend
  integer       , intent(in) :: iftp(:)
  real(8)       , intent(in) :: wfld(:),                                      &
  &                             wvct(:,jbgn:,kbgn:,lbgn:,:),                  &
  &                             wscl(  jbgn:,kbgn:,lbgn:,:),                  &
  &                             worn(:), wspc(:)

  character(30)  :: coff, ctpl, coff1, coff2, coff3, coff4
  character(100) :: corn, cspc
  integer :: ioff, ioff1, ioff2, ioff3, ioff4, i, ist, ied

  !byte count of variables
  integer, parameter :: bytes_int = 4
  integer, parameter :: bytes_flo = 8
  integer :: bytes_field, bytes_scalar, bytes_vector

  !type for data array
  character(7), parameter :: ctype = 'Float64'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

! bytes of functions
  bytes_scalar =   (jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo
  bytes_vector = 3*(jend-jbgn+1)*(kend-kbgn+1)*(lend-lbgn+1)*bytes_flo

! set bytes offset
  ioff1 = 0
  ioff2 = ioff1 + ifld* bytes_int + sum(iftp(1:ifld))*bytes_flo
  ioff3 = ioff2 + ivct*(bytes_int + bytes_vector)
  ioff4 = ioff3 + iscl*(bytes_int + bytes_scalar)

  write(coff1,'(i30)') ioff1
  write(coff2,'(i30)') ioff2
  write(coff3,'(i30)') ioff3
  write(coff4,'(i30)') ioff4

! grid origin and spacing
  write(corn,'(3(e23.16,1x,e23.16,1x,e23.16))') worn(1:3)
  write(cspc,'(3(e23.16,1x,e23.16,1x,e23.16))') wspc(1:3)

! writing
#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="ImageData" version="0.1"',                       &
  &          ' byte_order="LittleEndian">', eol
  write(iop) '<ImageData WholeExtent="'//cpnt//'" Origin="'//trim(corn)//'"', &
  &          ' Spacing="'//trim(cspc)//'">', eol

  !field data section
  write(iop) '<FieldData>', eol

  do i = 1, ifld

    ioff = ioff1 + (i-1)*bytes_int + sum(iftp(1:i-1))*bytes_flo
    write(coff,'(i30)') ioff
    write(ctpl,'(i30)') iftp(i)

    write(iop) '<DataArray type="'//ctype//'" Name="'//trim(flst(i))//'"',    &
    &          ' NumberOfTuples="'//trim(ctpl)//'"',                          &
    &          ' format="appended" offset="'//coff//'" />',eol

  end do

  !close field data section
  write(iop) '</FieldData>', eol

  !point data section
  write(iop) '<Piece Extent="'//cpnt//'">', eol
  write(iop) '<PointData>', eol

  !vector data declaration
  do i = 1, ivct

    !set offset
    ioff = ioff2 + (i-1)*(bytes_int+bytes_vector)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',             &
    & ' NumberOfComponents="3" format="appended" offset="'//coff//'" />', eol

  end do

  !scalar data declaration
  do i = 1, iscl

    !set offset
    ioff = ioff3 + (i-1)*(bytes_int+bytes_scalar)
    write(coff,'(i30)') ioff

    write(iop)                                                                &
    & '<DataArray type="'//ctype//'" Name="'//trim(slst(i))//'"',             &
    & ' format="appended" offset="'//coff//'" />', eol

  end do

  !close point data section
  write(iop) '</PointData>', eol

  !close tags
  write(iop) '</Piece>', eol
  write(iop) '</ImageData>', eol

  !declaration of appended section
  write(iop) '<AppendedData encoding="raw">', eol
  write(iop) '_'

  !write output value
  do i = 1, ifld
    ist = sum(iftp(1:i-1)) + 1
    ied = sum(iftp(1:i  ))
    bytes_field = bytes_flo*(ied-ist+1)
    write(iop) bytes_field, wfld(ist:ied)
  end do
  do i = 1, ivct
    write(iop) bytes_vector, wvct(1:3,jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do
  do i = 1, iscl
    write(iop) bytes_scalar, wscl(jbgn:jend,kbgn:kend,lbgn:lend,i)
  end do

  !close appended section
  write(iop) eol, '</AppendedData>', eol
  write(iop) '</VTKFile>'

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_ImageData_Double


!=================================================

  subroutine VTK_Writer_PImageData_Single(cnme,iop,nprocs,                     &
  &                                       vlst,slst,ivct,iscl,                 &
  &                                       bgns,ends,cn_lst,worn,wspc)
!-------------------------------------------------

  implicit none
  character(100), intent(in) :: cnme
  integer       , intent(in) :: iop
  integer       , intent(in) :: nprocs

  character(30) , intent(in) :: vlst(:), slst(:)
  integer       , intent(in) :: ivct, iscl
  integer       , intent(in) :: bgns(rs:,:), ends(rs:,:)
  character(100), intent(in) :: cn_lst(rs:)
  real(4)       , intent(in) :: worn(:), wspc(:)

  character(100) :: corn, cspc
  character(50)  :: cpnt
  character(4)   :: cprc
  integer :: re, i

  character(7), parameter :: ctype = 'Float32'

!-------------------------------------------------

  !end of line, CR+LF
  character :: eol(2)
  eol(1) = char(13); eol(2) = char(10)

!-------------------------------------------------

  re = nprocs-1

  !whole region
  write(cpnt,'(3(i7,1x,i7,1x))') bgns(rs,1), ends(re,1),                      &
  &                              bgns(rs,2), ends(re,2),                      &
  &                              bgns(rs,3), ends(re,3)

! grid origin and spacing
  write(corn,'(3(e15.8,1x,e15.8,1x,e15.8))') worn(1:3)
  write(cspc,'(3(e15.8,1x,e15.8,1x,e15.8))') wspc(1:3)

#ifdef sxf90
  open(iop,file=trim(cnme),form='unformatted',status='replace')
#else
  open(iop,file=trim(cnme),form='unformatted',status='replace',access='stream')
#endif

  !header
  write(iop) '<?xml version="1.0"?>', eol
  write(iop) '<VTKFile type="PImageData" version="0.1"',                      &
  &            ' byte_order="LittleEndian">', eol
  write(iop)'<PImageData WholeExtent="'//cpnt//'" GhostLevel="1"',            &
  &         ' Origin="'//trim(corn)//'" Spacing="'//trim(cspc)//'">', eol

  !point data section
  write(iop) '<PPointData>', eol

  !vector data declaration
  do i = 1, ivct
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(vlst(i))//'"',   &
    &          ' NumberOfComponents="3" />', eol
  end do

  !scalar data declaration
  do i = 1, iscl
    write(iop) '<PDataArray type="'//ctype//'" Name="'//trim(slst(i))//'" />', eol
  end do

  !close point data section
  write(iop) '</PPointData>', eol

  !table of indivisual pieces
  do i = rs, re
    write(cprc,'(i4.4)') i
    write(cpnt,'(3(i7,1x,i7,1x))') bgns(i,1), ends(i,1),                      &
    &                              bgns(i,2), ends(i,2),                      &
    &                              bgns(i,3), ends(i,3)
    write(iop) '<Piece Extent="'//cpnt//'" Source="'//trim(cn_lst(i))//'" />',&
    &          eol
  end do

  !close tags
  write(iop) '</PImageData>', eol
  write(iop) '</VTKFile>', eol

  close(iop)

!-------------------------------------------------
  end subroutine VTK_Writer_PImageData_Single

end module mod_VTK_IO
