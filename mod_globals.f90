module mod_globals

  !================================IO==============================
  integer, save :: lread
  integer, parameter :: lwrite = 6
  character(:), allocatable, save :: bfilcmd, boutpfx
  character(30), allocatable :: boutsfx(:)
  integer, save :: hFileList ! unit like File Handle


  !================================FlowFileInfo====================
  integer, save :: ljbgn, ljend, lkbgn, lkend, llbgn, llend
  integer, save :: ljlen, lklen, lllen, ljklsum
  integer, save :: lfld, lvct, lscl, ltimidx
  integer, allocatable, save :: lscllst, bscllst

  !================================Process=========================
  integer, save :: lprc

end module
