module mod_szk
!ver 1.00
!    180707


  character(*), parameter, private :: findlist = '_findlist_szk_mod.txt'
  integer, parameter, private :: usedunitmax = 50
  integer, private :: usedunit(usedunitmax)


contains
  ! toString===============
  !** 整数を文字列へ
  ! * i      整数
  ! * return 文字列
  !========================
  function toString(i)
    integer, intent(in) :: i
    character(:), allocatable :: toString
    character(20) :: str
    write(str, *) i
    allocate(character(len_trim(str)) :: toString)
    write(toString, *) i
  endfunction

  ! toInteger==============
  !** 文字列を整数へ
  ! * a      文字列
  ! * return 整数
  !========================
  function toInteger(a)
    character(*), intent(in) :: a
    integer :: toInteger
    read(a, *) toInteger
  endfunction

  ! in_countstr===============
  !** (内部関数)文字列検索
  ! * a      テキスト全文
  ! * s      検索文字列
  ! * an     $aのバイト数
  ! * sn     $sのバイト数
  ! * return $aの中にある$sの数
  !========================
  function in_countstr(a, s, an, sn)
    integer, intent(in) :: an, sn
    character(an) :: a
    character(sn) :: s
    integer :: ls, i , ii
    integer :: in_countstr
    ls = len(s)
    in_countstr = 0
    ii = len(a) - ls + 1
    do i=1, ii
      if(a(i:i+ls-1) == s) in_countstr = in_countstr + 1
    enddo
  endfunction

  ! split===============
  !** 文字列分割
  ! * a        テキスト全文
  ! * s        検索文字列
  ! * idx      何文字目から検索か
  ! * idx(out) 次は何文字目から検索すべきか(0: 終端)
  ! * return   $aの$idx文字目から，$sが現れる直前or終端までの文字列(未trim)
  !========================
  function split(a, s, idx)
    character(*), intent(in) :: a, s
    character(:), allocatable:: split
    integer, intent(inout) :: idx
    integer :: n
    integer :: i, ii, ls, la
    la = len(a)
    ls = len(s)
    ii = la - ls + 1
    i = idx
    do
      if(a(i:i+ls-1)==s)then
        allocate(character(i-idx) :: split)
        split = a(idx:i-1)
        idx = i + ls
        if(idx > la) idx = 0
        exit
      else if(i == ii)then
        split = a(idx:la)
        idx = 0
        exit
      endif
      i = i + 1
    enddo
  endfunction
  
  function openFileListStream(command)
    character(*), intent(in), optional :: command
    integer :: openFileListStream
    if(present(command)) then
      call system( command // ' > ' // findlist)
    else
      call system('ls > ' // findlist)
    endif
    openFileListStream = open2(findlist, 'old', 'formatted')
  endfunction
  function closeFindListStream(unit)
    integer, intent(in) :: unit
    integer :: closeFindListStream
    closeFindListStream = close2(unit)
    call system('rm ' // findlist)
  endfunction
  function lock2()
    integer :: lock2
    integer :: i
    lock2 = 0
    do i=1, usedunitmax
      if(usedunit(i)==0)then
        usedunit(i) = 1
        lock2 = 20 + i
        return
      endif
    enddo
    write(6,*) 'ERROR MOD_SZK !! Cannot open file!'
  endfunction
  function release2(unit)
    integer, intent(in) :: unit
    integer :: release2
    release2 = unit
    usedunit(unit - 20) = 0
  endfunction
  function open2(file , status, form)!lockに統合？
    integer :: open2
    character(len=*), intent(in) :: file
    character(len=*), intent(in), optional :: status, form
    open2 = lock2()
    if(open2/=0)then
      open(unit=open2, file=file, status=status, form=form)
      return
    endif
    write(6,*) 'ERROR MOD_SZK !! Cannot open file!'
  endfunction
  function close2(unit)
    integer, intent(in) :: unit
    integer :: close2
    close(unit)
    close2 = release2(unit)
  endfunction

end module mod_szk
