module mod_globals

  implicit none

  !================================Process=========================
  integer, save :: lprc
  character(30), parameter :: bprclst(2) = (/ 'searchShock', 'shockHeight' /)
  integer, parameter :: lprcoutcol(2) =    (/ 6            , 1             /)




  !================================IO==============================
  integer, save :: lread
  integer, parameter :: lwrite = 6
  character(:), allocatable, save :: bfilcmd, boutpfx
  character(30), allocatable :: boutsfx(:)
  integer, save :: fg_loop
  integer, save :: hFileList ! unit like File Handle

  !================================FlowFileInfo====================
  integer, save :: ljbgn, ljend, lkbgn, lkend, llbgn, llend
  integer, save :: ljlen, lklen, lllen, ljklsum
  integer, save :: lfld, lvct, lscl, ltimidx
  integer, allocatable, save :: lscllst(:)
  character(30), allocatable, save :: bscllst(:)
  integer, save :: lallfld, lallvct, lallscl
  character(30), allocatable, save :: ballfldlst(:), ballvctlst(:), ballscllst(:)
  integer, allocatable, save :: lallftplst(:)

  !================================Common Calculation==============
  real(4), allocatable, save :: sfld(:), svct(:,:,:,:,:), sscl(:,:,:,:), sgrd(:,:,:,:)
  real(4), allocatable, save :: sout(:,:)
  integer, allocatable, save :: lftp(:)

end module
