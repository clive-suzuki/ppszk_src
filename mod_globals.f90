module mod_globals

  !================================Process=========================
  integer, save :: lprc
  character(30), parameter :: bprclst = (/ 'searchShock' /)
  integer, parameter :: lprcoutcol =    (/ 6             /)




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
  integer, save :: lallfld, lallvct, lallscl
  character(30), allocatable, save :: lallfldlst(:), lallvctlst(:), lallscllst(:)

  !================================Common Calculation==============
  real(4), allocatable, save :: sfld(:), svct(:,:,:,:,:), sscl(:,:,:,:), sgrd(:,:,:,:)
  real(4), allocatable, save :: sout(:,:)
  integer, allocatable, save :: lftp(:)

end module
