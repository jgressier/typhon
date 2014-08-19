!------------------------------------------------------------------------------!
!> @brief FLUENT module, definition, reading routines
!! FLUENT format is detailed on
!! http://aerojet.engr.ucdavis.edu/fluenthelp/html/ug/node1490.htm
!------------------------------------------------------------------------------!
! f=readmsh ; m=cone.msh ; ifort -o $f $f.f90 && ./$f <<< $m
module FLUENT

use IO_UNIT
use IOCFD
use STRING
use USTMESH

implicit none

! -- Global Variables -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

integer, parameter :: kfp = 4

character(len=262144):: stropt
character(len=4096) :: namoptlist
character(len=64)   :: namopt
integer             :: indopt
integer             :: hedopt
integer,parameter   :: lenopt   = 10
integer             :: incopt
integer,parameter   :: lennames = 30

integer :: espion

character,parameter :: nl_char = achar(10)
character,parameter :: cr_char = achar(13)

!------------------------------------------------------------------------------!
! CONSTANTS for FLUENT ELEMENT TYPE
!------------------------------------------------------------------------------!

! Section headers
integer(kfp), parameter :: fluent_sct_comment   =  0
integer(kfp), parameter :: fluent_sct_header    =  1
integer(kfp), parameter :: fluent_sct_dim       =  2
integer(kfp), parameter :: fluent_sct_nodes      =   10
integer(kfp), parameter :: fluent_sct_spbinnodes = 2010
integer(kfp), parameter :: fluent_sct_dpbinnodes = 3010
integer(kfp), parameter :: fluent_sct_cells      =   12
integer(kfp), parameter :: fluent_sct_spbincells = 2012
integer(kfp), parameter :: fluent_sct_faces      =   13
integer(kfp), parameter :: fluent_sct_spbinfaces = 2013
integer(kfp), parameter :: fluent_sct_per       = 18
integer(kfp), parameter :: fluent_sct_zonebcs   = 39
integer(kfp), parameter :: fluent_sct_zonebnd   = 45
integer(kfp), parameter :: fluent_sct_ctree     = 58
integer(kfp), parameter :: fluent_sct_ftree     = 59
integer(kfp), parameter :: fluent_sct_fparent   = 61

! Ignored section headers
integer(kfp), parameter :: fluent_sct_ignore1   =  4

! Cell types
integer(kfp), parameter :: fluent_cells_mixed   =  0
integer(kfp), parameter :: fluent_cells_tri     =  1
integer(kfp), parameter :: fluent_cells_tetra   =  2
integer(kfp), parameter :: fluent_cells_quad    =  3
integer(kfp), parameter :: fluent_cells_hexa    =  4
integer(kfp), parameter :: fluent_cells_pyram   =  5
integer(kfp), parameter :: fluent_cells_wedge   =  6
integer(kfp), parameter :: fluent_cells_polyh   =  7

! BC types
integer(kfp), parameter :: fluent_bc_interior   =  2
integer(kfp), parameter :: fluent_bc_wall       =  3
integer(kfp), parameter :: fluent_bc_p_inlet    =  4
integer(kfp), parameter :: fluent_bc_p_outlet   =  5
integer(kfp), parameter :: fluent_bc_symmetry   =  7
integer(kfp), parameter :: fluent_bc_per_sdow   =  8
integer(kfp), parameter :: fluent_bc_p_farfld   =  9
integer(kfp), parameter :: fluent_bc_v_inlet    = 10
integer(kfp), parameter :: fluent_bc_periodic   = 12
integer(kfp), parameter :: fluent_bc_fan        = 14
integer(kfp), parameter :: fluent_bc_mflow_in   = 20
integer(kfp), parameter :: fluent_bc_interfce   = 24
integer(kfp), parameter :: fluent_bc_parent     = 31
integer(kfp), parameter :: fluent_bc_outflow    = 36
integer(kfp), parameter :: fluent_bc_axis       = 37

! Face types
integer(kfp), parameter :: fluent_faces_mixed   =  0
integer(kfp), parameter :: fluent_faces_linear  =  2
integer(kfp), parameter :: fluent_faces_tri     =  3
integer(kfp), parameter :: fluent_faces_quad    =  4
integer(kfp), parameter :: fluent_faces_polyg   =  5

! -- Private data --

! -- Parameters --

interface str_to_int
  module procedure str_to_intkfp
endinterface

! -- Data types --

!------------------------------------------------------------------------------!
! ST_DEFFLUENT
!------------------------------------------------------------------------------!
type st_deffluent
  integer       :: version
  integer       :: iu_bin
  logical       :: binary
  integer       :: spcdim
  integer       :: inmin, inmax, nnodes
  !integer       :: icmin, icmax, ncells
  real*4, allocatable :: xyzsp(:,:)
  real*8, allocatable :: xyzdp(:,:)
  !integer*4, allocatable :: celltype(:)
  integer       :: nelem, nbc
  character(len=64) :: zonename !WARNING: MAY HAVE TO BE AN ARRAY
  character         :: c        ! one character read in advance
  character(len=lennames), allocatable :: bcnames(:)
end type st_deffluent


!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! @brief transfer element type
!------------------------------------------------------------------------------!
integer function fluent2typhon_elemtype(etype)

  implicit none
  ! -- INPUTS --
  integer(kpp),      intent(in)  :: etype
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(etype)
  case(fluent_cells_tri)
    fluent2typhon_elemtype = elem_tri3
  case(fluent_cells_quad)
    fluent2typhon_elemtype = elem_quad4
  case(fluent_cells_polyh)
    fluent2typhon_elemtype = elem_ngon
  case(fluent_cells_tetra)
    fluent2typhon_elemtype = elem_tetra4
  case(fluent_cells_pyram)
    fluent2typhon_elemtype = elem_pyra5
  case(fluent_cells_wedge)
    fluent2typhon_elemtype = elem_penta6
  case(fluent_cells_hexa)
    fluent2typhon_elemtype = elem_hexa8
  case default
    fluent2typhon_elemtype = -1
  endselect

endfunction fluent2typhon_elemtype

!------------------------------------------------------------------------------!
!> @brief open FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_openread(filename, deffluent)

  implicit none
  ! -- INPUTS --
  character(len=*)   , intent(in)  :: filename
  ! -- OUTPUTS --
  type(st_deffluent) , intent(out) :: deffluent
  ! -- Internal variables --
  integer :: info
  ! -- BODY --

  deffluent%iu_bin = getnew_io_unit()
  OPEN(UNIT   = deffluent%iu_bin, &
       FILE   = filename, &
       ACCESS = "STREAM", &
       FORM   = "UNFORMATTED", CONVERT = 'LITTLE_ENDIAN', &
       IOSTAT = info)
  if (info /= 0) then
    call cfd_error("(fluent) unable to open unformatted file "//trim(filename))
  endif

  read(deffluent%iu_bin) deffluent%c

endsubroutine fluent_openread

!------------------------------------------------------------------------------!
!> @brief close FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_close(deffluent)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(in) :: deffluent
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  call close_io_unit(deffluent%iu_bin)

endsubroutine fluent_close


!------------------------------------------------------------------------------!
character        function chnl(c)
  implicit none
  character        :: c
  select case(ichar(c))
  case(0)
    chnl = " "
  case(10, 13)
    chnl = "\"
  case default
    chnl = c
  endselect
endfunction chnl

!------------------------------------------------------------------------------!
subroutine dumpp()
  implicit none
  call flush(6)
  write(6,'(a)') trim(stropt)
  call flush(6)
endsubroutine dumpp

!------------------------------------------------------------------------------!
subroutine stopp(iu, c)
  implicit none
  integer :: i, iu
  character        :: c
  character(len=2) :: str
  do i = 1,16
    read(iu) c
    write(str,'(z0.2)') ichar(c)
    call affline("follow: c='"//chnl(c)//"'" &
               //" (0x"//str//")")
  enddo
  call dumpp()
  stop
endsubroutine stopp

!------------------------------------------------------------------------------!
subroutine affstarp(optname)
  implicit none
  character(len=*) :: optname
  hedopt = hedopt+1
  write(namopt,'(a)') " "//optname
  write(namoptlist(lenopt* hedopt+1 : &
                   lenopt*(hedopt+1)),'(a)') namopt(1:lenopt)
  incopt = 1
  call affline("(start)")
  incopt = 0
endsubroutine affstarp

!------------------------------------------------------------------------------!
subroutine affexitp(optname)
  implicit none
  character(len=*) :: optname
  incopt = 1
  call affline("(exit)")
  incopt = 0
  hedopt = hedopt-1
  write(namopt,'(a)') namoptlist(lenopt* hedopt+1 : &
                                 lenopt*(hedopt+1))
endsubroutine affexitp

!------------------------------------------------------------------------------!
subroutine affstar(optname, c)
  implicit none
  character(len=*) :: optname
  character        :: c
  hedopt = hedopt+1
  write(namopt,'(a)') " "//optname
  write(namoptlist(lenopt* hedopt+1 : &
                   lenopt*(hedopt+1)),'(a)') namopt(1:lenopt)
  incopt = 1
  call affline("(start: c='"//chnl(c)//"')")
  incopt = 0
endsubroutine affstar

!------------------------------------------------------------------------------!
subroutine affexit(optname, c)
  implicit none
  character(len=*) :: optname
  character        :: c
  incopt = 1
  call affline("(exit:  c='"//chnl(c)//"')")
  incopt = 0
  hedopt = hedopt-1
  write(namopt,'(a)') namoptlist(lenopt* hedopt+1 : &
                                 lenopt*(hedopt+1))
endsubroutine affexit

!------------------------------------------------------------------------------!
subroutine affline(str)
  implicit none
  character(len=*) :: str
  character(len=1024), parameter :: blkstr = " "
  write(stropt(indopt:len(stropt)),'(7a)') &
    blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
    blkstr(1:2*hedopt),trim(str),nl_char
  indopt = indopt &
         + 2*hedopt + 1 + lenopt + 2 &
         + 2*hedopt + len(str) + 1
endsubroutine affline

!------------------------------------------------------------------------------!
subroutine affhead(str)
  implicit none
  character(len=*) :: str
  character(len=1024), parameter :: blkstr = " "
  write(stropt(indopt:len(stropt)),'(6a)') &
    blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
    blkstr(1:2*hedopt),trim(str)
  indopt = indopt &
         + 2*hedopt + 1 + lenopt + 2 &
         + 2*hedopt + len(str)
endsubroutine affhead

!------------------------------------------------------------------------------!
subroutine affcont(str)
  implicit none
  character(len=*) :: str
  write(stropt(indopt:len(stropt)),'(a)') &
    trim(str)
  indopt = indopt + len(str)
endsubroutine affcont

!------------------------------------------------------------------------------!
subroutine afftail(str)
  implicit none
  character(len=*) :: str
  write(stropt(indopt:len(stropt)),'(2a)') &
    trim(str), nl_char
  indopt = indopt + len(str) + 1
endsubroutine afftail

!------------------------------------------------------------------------------!
subroutine str_index_spc(str, indspc, nspc)
  implicit none
  character(len=*) , intent(in)  :: str
  integer          , intent(out) :: indspc(*)
  integer          , intent(out) :: nspc
  integer          :: i, prevspc
  nspc = 0
  prevspc = 1

  i = 1
  !do while (.true.)
  !  do while ( str(i:i) /= ' ' )
  !    i = i + 1
  !    if (i>len_trim(str) return
  !  enddo
  !enddo

  do i = 1,len_trim(str)
    if ( str(i:i) == ' ' ) then
      if ( prevspc == 0 ) then
        nspc = nspc+1
        !if 
        indspc(nspc) = i
      endif
      prevspc = 1
    else
      prevspc = 0
    endif
  enddo
  if ( prevspc == 0 ) then
    nspc=nspc+1
    indspc(nspc) = i-1
  endif
endsubroutine str_index_spc

!------------------------------------------------------------------------------!
subroutine str_read_ints(str, formt, inb, ilist, ierr)
  implicit none
  character(len=*) , intent(in)  :: str
  character(len=*) , intent(in)  :: formt
  integer          , intent(out) :: inb
  integer(kfp)     , intent(out) :: ilist(*)
  integer          , intent(out) :: ierr
  integer          :: indspc(64), nspc
  integer          :: ispc, ii
  call str_index_spc(str, indspc, nspc)
  ii = 1
  inb = 0
  do ispc = 1, nspc
    inb = inb+1
    ilist(inb) = str_to_intkfp(str(ii:indspc(ispc)), formt, ierr)
    if ( ierr /= 0 ) then
      call cfd_warning("could not read integer #"//trim(strof(ispc)) &
                     //" from: '"//trim(str)//"'")
      return
    endif
    ii =indspc(ispc)+1
  enddo
endsubroutine str_read_ints

!------------------------------------------------------------------------------!
integer(kfp) function str_to_intkfp(str, formt, ierr)
  implicit none
  character(len=*) :: str
  character(len=*) :: formt
  integer(kfp) :: ivalue
  integer :: ierr
  read(str,formt,iostat=ierr) ivalue
  if ( ierr /= 0 ) then
    call cfd_warning("could not read integer" &
                   //" from: '"//trim(str)//"'" &
                   //" by:   '"//trim(formt)//"'")
    return
  endif
  str_to_intkfp = ivalue
endfunction str_to_intkfp

!------------------------------------------------------------------------------!
character(len=16) function chardesc(c)
  implicit none
  character        :: c
  select case(c)
  case(' ')
    chardesc = "space"
  case(nl_char)
    chardesc = "nline"
  case(cr_char)
    chardesc = "cgrtn"
  case default
    chardesc = "char"
  endselect
endfunction chardesc

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level subroutines
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : skip blank (space or newline)
!------------------------------------------------------------------------------!
subroutine fll_ignore_blank(iu, c)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("ignore_b_", c)
  do while ( c == ' ' &
        .or. c == nl_char &
        .or. c == cr_char )
    call affline("'"//chnl(c)//"'"// &
                 " "//trim(chardesc(c))//" skipped")
    read(iu) c
  enddo
  call affexit("ignore_b_", c)
endsubroutine fll_ignore_blank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store char list
!------------------------------------------------------------------------------!
subroutine fll_storecharlist(iu, c, clist, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character(len=*) , intent(in)    :: clist
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i
  ! -- BODY --
  !call affstar("store_dec", c)
  str = ""
  i = 0
  !call affhead("'")
  do while ( scan(clist,c) /= 0 )
  !call affcont(c)
    i = i+1
    str(i:i) = c
    read(iu) c
  enddo
  !call afftail("' stored")
  !call affexit("store_dec", c)
endsubroutine fll_storecharlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store decimal digits
!------------------------------------------------------------------------------!
#define fll_storedecdigits(iu, c, str) fll_storecharlist(iu, c, "0123456789", str)
!subroutine fll_storedecdigits(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  !integer :: i
!  ! -- BODY --
!  call fll_storecharlist(iu, c, "0123456789", str)
!!  !call affstar("store_dec", c)
!!  str = ""
!!  i = 0
!!  !call affhead("'")
!!  do while ( c >= '0' .and. c <= '9' )
!!  !call affcont(c)
!!    i = i+1
!!    str(i:i) = c
!!    read(iu) c
!!  enddo
!!  !call afftail("' stored")
!!  !call affexit("store_dec", c)
!endsubroutine fll_storedecdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store hexadecimal digits
!------------------------------------------------------------------------------!
#define fll_storehexdigits(iu, c, str) fll_storecharlist(iu, c, "0123456789abcdef", str)
!subroutine fll_storehexdigits(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  !integer :: i
!  ! -- BODY --
!  call fll_storecharlist(iu, c, "0123456789abcdef", str)
!!  !call affstar("store_hex", c)
!!  str = ""
!!  i = 0
!!  !call affhead("'")
!!  do while ( ( c >= '0' .and. c <= '9' ) &
!!        .or. ( c >= 'a' .and. c <= 'f' ) )
!!  !call affcont(c)
!!    i = i+1
!!    str(i:i) = c
!!    read(iu) c
!!  enddo
!!  !call afftail("' stored")
!!  !call affexit("store_hex", c)
!endsubroutine fll_storehexdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to list of chars
!------------------------------------------------------------------------------!
subroutine fll_storeuptocharlist(iu, c, clist, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character(len=*) , intent(in)    :: clist
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i, lstr
  ! -- BODY --
  !call affstar("store_chl", c)
  str = ""
  lstr = len(str)
  i = 0
  !call affhead("'")
  do while ( scan(clist,c) == 0 )
  !call affcont(c)
    i = i+1
    if ( i > lstr ) then
      call stopp(iu, c)
    endif
    str(i:i) = c
    read(iu) c
  enddo
  !call affcont("' stored")
  !call afftail(" ; str='"//trim(str)//"'")
  !call affexit("store_chl", c)
endsubroutine fll_storeuptocharlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to separator (blank or bracket)
!------------------------------------------------------------------------------!
#define fll_storeuptosepbb(iu, c, str) fll_storeuptocharlist(iu, c, "() "//nl_char//cr_char, str)
!subroutine fll_storeuptosepbb(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  integer :: i
!  ! -- BODY --
!  !call affstar("store_sep", c)
!  str = ""
!  i = 0
!  !call affhead("'")
!  do while ( c /= ' ' &
!       .and. c /= nl_char &
!       .and. c /= cr_char &
!       .and. c /= '(' &
!       .and. c /= ')' )
!  !call affcont(c)
!    i = i+1
!    str(i:i) = c
!    read(iu) c
!  enddo
!  !call affcont("' stored")
!  !call afftail(" ; str='"//trim(str)//"'")
!  !call affexit("store_sep", c)
!endsubroutine fll_storeuptosepbb

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to blank (space or newline)
!------------------------------------------------------------------------------!
#define fll_storeuptoblank(iu, c, str) fll_storeuptocharlist(iu, c, " "//nl_char//cr_char, str)
!subroutine fll_storeuptoblank(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  integer :: i
!  ! -- BODY --
!  !call affstar("store_nb_", c)
!  str = ""
!  i = 0
!  !call affhead("'")
!  do while ( c /= ' ' &
!       .and. c /= nl_char &
!       .and. c /= cr_char )
!  !call affcont(c)
!    i = i+1
!    str(i:i) = c
!    read(iu) c
!  enddo
!  !call affcont("' stored")
!  !call afftail(" ; str='"//trim(str)//"'")
!  !call affexit("store_nb_", c)
!endsubroutine fll_storeuptoblank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_storeuptocharnostep(iu, c, cc, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i, lstr
  ! -- BODY --
  !call affstar("store_chr", c)
  str = ""
  lstr = len(str)
  i = 0
  !call affhead("'")
  do while ( c /= cc )
  !call affcont(c)
    i = i+1
    if ( i > lstr ) then
      call stopp(iu, c)
    endif
    str(i:i) = c
    read(iu) c
  enddo
  !call affcont("' stored")
  !call afftail(" ; str='"//trim(str)//"'")
  !call affexit("store_chr", c)
endsubroutine fll_storeuptocharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!------------------------------------------------------------------------------!
subroutine fll_storeuptochar(iu, c, cc, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  !call affstar("store_chr", c)
  call fll_storeuptocharnostep(iu, c, cc, str)
  read(iu) c
  !call affline("next : '"//c//"'")
  !call affexit("store_chr", c)
endsubroutine fll_storeuptochar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_checknextcharnostep(iu, c, cc)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("chknxnstp", c)
  call fll_ignore_blank(iu, c)
  if ( c /= cc ) then
    call affline("cc = '"//cc//"'")
    call affline("c  = '"//c //"'")
    write(6,'(a)') "Error !"
    call stopp(iu, c)
  endif
  call affline("'"//chnl(c)//"' checked")
  call affexit("chknxnstp", c)
endsubroutine fll_checknextcharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!------------------------------------------------------------------------------!
subroutine fll_checknextchar(iu, c, cc)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("check_nxt", c)
  call fll_checknextcharnostep(iu, c, cc)
  read(iu) c
  call affexit("check_nxt", c)
endsubroutine fll_checknextchar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read integer
!------------------------------------------------------------------------------!
subroutine fll_getint(iu, c, val)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  integer          , intent(out)   :: val
  ! -- Internal variables --
  character(len=256) :: str
  integer :: ierr
  ! -- BODY --
  call affstar("get_int__", c)
  call fll_ignore_blank(iu, c)
  call fll_storedecdigits(iu, c, str)
  call affline("read integer from string : '"//trim(str)//"'")
  val = str_to_int(str, '(i)', ierr)
  ! Error could be put inside str_to_int
  if ( ierr /= 0 ) then
    write(6,'(a)') "Error !"
    call stopp(iu, c)
  endif
  call affline("integer read : "//trim(str))
  call affexit("get_int__", c)
endsubroutine fll_getint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read hexadecimal integer
!------------------------------------------------------------------------------!
subroutine fll_gethexint(iu, c, val)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  integer(kfp)     , intent(out)   :: val
  ! -- Internal variables --
  character(len=256) :: str
  integer :: ierr
  ! -- BODY --
  call affstar("get_hex__", c)
  call fll_ignore_blank(iu, c)
  call fll_storeuptoblank(iu, c, str)
  call affline("read integer from string : '"//trim(str)//"'")
  val = str_to_int(str, '(z)', ierr)
  if ( ierr /= 0 ) then
    write(6,'(a)') "Error !"
    call stopp(iu, c)
  endif
  write(str,'(i)') val
  call affline("integer read : "//trim(str))
  call affexit("get_hex__", c)
endsubroutine fll_gethexint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read string
!------------------------------------------------------------------------------!
subroutine fll_getstring(iu, c, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("get_str__", c)
  call fll_checknextchar(iu, c, '"')
  call fll_storeuptochar(iu, c, '"', str)
  call affline("read string : '"//trim(str)//"'")
  call affexit("get_str__", c)
endsubroutine fll_getstring

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read list
!------------------------------------------------------------------------------!
subroutine fll_getlist(iu, c, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("getlist_", c)
  call fll_checknextchar(iu, c, '(')
  call fll_storeuptochar(iu, c, ')', str)
  call affline("read list : '"//trim(str)//"'")
  call affexit("getlist_", c)
endsubroutine fll_getlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : extract list
!------------------------------------------------------------------------------!
subroutine fll_extractlist(iu, c, strlistsize, strlist)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  integer          , intent(inout) :: strlistsize
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: strlist(strlistsize)
  ! -- Internal variables --
  integer           :: nstr, inst, lstr
  character         :: s
  ! -- BODY --
  call affstar("extract__", c)
  call fll_checknextchar(iu, c, '(')
  do nstr = 1,strlistsize
    inst = 0
    strlist(nstr) = ""
    call fll_ignore_blank(iu, c)
    if ( c == ')' ) exit
    call fll_storeuptosepbb(iu, c, strlist(nstr))
  write(s,'(i1)') nstr
  call affline("strl["//s//"] = '"//trim(strlist(nstr))//"'")
    inst = len_trim(strlist(nstr))
  enddo
  nstr = nstr-1
  write(6,'(a,i4,a,$)') "nstr =",nstr,"    "
  strlistsize = nstr
  call fll_checknextchar(iu, c, ')')
  call affexit("extract__", c)
endsubroutine fll_extractlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level subroutines end
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!> @brief FLUENT : read string list
!------------------------------------------------------------------------------!
subroutine fluent_read_strlist(def, str)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  character(len=*)   , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("get_list_", def%c)
  call fll_getlist(def%iu_bin, def%c, str)
  call affexit("get_list_", def%c)

endsubroutine fluent_read_strlist

!------------------------------------------------------------------------------!
!> @brief FLUENT : read integer list
!------------------------------------------------------------------------------!
subroutine fluent_read_intlist(def, formt, inb, ilist, str)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  character(len=*)   , intent(in)    :: formt
  ! -- OUTPUTS --
  integer            , intent(out)   :: inb
  integer(kfp)       , intent(out)   :: ilist(*)
  character(len=*)   , intent(out)   :: str
  ! -- Internal variables --
  integer :: ierr
  ! -- BODY --
  call affstar("get_list_", def%c)
  call fll_getlist(def%iu_bin, def%c, str)
  call str_read_ints(str, formt, inb, ilist, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("(fluent) could not read integer list")
  endif
  call affexit("get_list_", def%c)

endsubroutine fluent_read_intlist

!------------------------------------------------------------------------------!
!> @brief FLUENT : check space dimension
!------------------------------------------------------------------------------!
subroutine fluent_check_spcdim(spcdim, iswrite)

  implicit none
  ! -- INPUTS --
  integer , intent(in) :: spcdim
  integer , intent(in) , optional :: iswrite
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  select case(spcdim)
  case(2)
    if ( present(iswrite) .and. iswrite == 1 ) &
    call cfd_print("    mesh is 2D (planar)")
  case(3)
    if ( present(iswrite) .and. iswrite == 1 ) &
    call cfd_print("    mesh is 3D")
  case default
    call cfd_error("(fluent) unknown space dimension: " &
                   //trim(strof(spcdim)))
  endselect

endsubroutine fluent_check_spcdim

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section id (and leading '(')
!------------------------------------------------------------------------------!
subroutine fluent_get_sctid(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  integer            , intent(out)   :: sctid
  ! -- Internal variables --
  character(len=64)  :: str
  integer :: ierr
  ! -- BODY --
  call affstar("get_s_id_", def%c)
  call fll_checknextchar(def%iu_bin, def%c, '(')
  call fll_getint(def%iu_bin, def%c, sctid)
  call affline("read section head : '"//trim(strof(sctid))//"'")
  call affexit("get_s_id_", def%c)

endsubroutine fluent_get_sctid

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : comment
!------------------------------------------------------------------------------!
subroutine fluent_get_comment(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=64)  :: str
  integer            :: isname = 0
  ! -- BODY --
  call fll_getstring(def%iu_bin, def%c, str)
  if ( str(1:22) == "Interior faces of zone" ) then
    def%zonename = trim(str(24:))
    isname = 1
  elseif ( str(1:13) == "Faces of zone" ) then
    def%zonename = trim(str(15:))
    isname = 1
  endif
  if ( isname == 1 ) then
    call cfd_print(' '//trim(str))
  else
    call cfd_write('"'//trim(str)//'"')
  endif

endsubroutine fluent_get_comment

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : header
!------------------------------------------------------------------------------!
subroutine fluent_get_header(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=64) :: str
  ! -- BODY --
  call fll_getstring(def%iu_bin, def%c, str)
  write(6,'(3a)') "Header    : '",trim(str),"'"

endsubroutine fluent_get_header

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : space dimension
!------------------------------------------------------------------------------!
subroutine fluent_get_spcdim(def, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  ! -- BODY --
  call fll_getint(def%iu_bin, def%c, def%spcdim)
  if ( umesh%elemdim == 0 ) then
    continue
  elseif ( umesh%elemdim == def%spcdim ) then
    call cfd_warning("space dimension already set")
  else
    call cfd_warning("space dimension already set to " &
                     //trim(strof(umesh%elemdim)))
  endif
  umesh%elemdim = def%spcdim
  call fluent_check_spcdim(def%spcdim, 1)

endsubroutine fluent_get_spcdim

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : nodes
!------------------------------------------------------------------------------!
subroutine fluent_get_nodes(def, sctid, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer           :: zoneid
  integer           :: inmin, inmax, ndim
  integer           :: itype
  integer           :: ityperef
  character(len=64) :: strzone
  character(len=64) :: strtype
  character(len=64) :: str, strref
  integer           :: i, inb
  integer           :: j
  integer(kfp)      :: ilist(5)
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str)

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = def%spcdim
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  inmin  = ilist(2)
  inmax  = ilist(3)
  itype  = ilist(4)
  ndim   = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
  else
    write(strzone,'(a5,z4)') "zone ",zoneid
    if ( ndim == 2 .or. def%spcdim == 2 ) then
      strtype = " x, y"
    elseif ( ndim == 2 .or. def%spcdim == 3 ) then
      strtype = " x, y, z"
    else
      strtype = ""
    endif
  endif
  write(str,'(3a,2(i8,a))') "(",trim(strzone),"): (",inmin,":",inmax,")"
  call cfd_print(" - reading FLUENT nodes "//trim(str)//trim(strtype))

  ! Check itype
  if ( zoneid == 0 ) then
    ityperef = 0
  else
    ityperef = 1
  endif
  if ( itype /= ityperef ) then
    call cfd_warning("(fluent) type = "//trim(strof(itype)) &
                        //" /= typeref = "//trim(strof(ityperef)))
  endif

  ! Check space dimension
  if ( def%spcdim == 0 ) then
    if ( ndim == 0 ) then
      if ( zoneid == 0 ) then
        call cfd_warning("(fluent) spcdim = 0 and ndim = 0")
      else
        call cfd_error("(fluent) spcdim = 0 and ndim = 0")
      endif
    else
      if ( zoneid == 0 ) then
        call cfd_warning("(fluent) spcdim = 0 , set to ndim ="//trim(strof(ndim)))
        def%spcdim = ndim
        umesh%elemdim = def%spcdim
      else
        call cfd_warning("(fluent) spcdim = 0 , use ndim ="//trim(strof(ndim)))
      endif
    endif
  elseif ( ndim /= def%spcdim ) then
    call cfd_write("Error: spcdim = "//trim(strof(def%spcdim)))
    call cfd_write("       ndim   = "//trim(strof(ndim)))
    call cfd_error("wrong space dimension of zone"//trim(strzone))
  endif

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then

    def%inmin = inmin
    def%inmax = inmax
    def%nnodes = inmax-inmin+1

    umesh%nvtex = inmax-inmin+1
    umesh%mesh%nvtex = umesh%nvtex
    umesh%mesh%idim = umesh%nvtex
    umesh%mesh%jdim = 1
    umesh%mesh%kdim = 1

    ! Allocate umesh vertices
    allocate(umesh%mesh%vertex(umesh%mesh%idim, umesh%mesh%jdim, umesh%mesh%kdim))

  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then

    ! Check allocation of vertex array
    if ( umesh%mesh%nvtex /= umesh%nvtex ) then
      call cfd_error("umesh%mesh%vertex is not allocated")
    elseif ( inmax > umesh%nvtex ) then
      call cfd_write("Error: nvtex = "//trim(strof(umesh%nvtex)))
      call cfd_write("       inmax = "//trim(strof(inmax))//" > nvtex")
      call cfd_error("wrong dimension of vertex array")
    endif

    ! Allocate local xyz array
    select case(sctid)
    case(fluent_sct_spbinnodes)
      allocate(def%xyzsp(2,inmax-inmin+1))
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      allocate(def%xyzdp(2,inmax-inmin+1))
    endselect

    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

    ! Read vertices
    select case(sctid)
    ! Ascii values
    case(fluent_sct_nodes)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)
      do i = 1,inmax-inmin+1
        call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
        read(str,*) (def%xyzdp(j,i),j=1,ndim)
      enddo
    ! Binary values (simple precision)
    case(fluent_sct_spbinnodes)
      read(def%iu_bin) ((def%xyzsp(i,j),j=1,ndim),i=1,inmax-inmin+1)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    ! Binary values (double precision)
    case(fluent_sct_dpbinnodes)
      read(def%iu_bin) ((def%xyzdp(i,j),j=1,ndim),i=1,inmax-inmin+1)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')

    ! Transfer vertices
    select case(sctid)
    case(fluent_sct_spbinnodes)
      do i = inmin, inmax
        umesh%mesh%vertex(i,1,1)%x = def%xyzsp(1,i-inmin+1)
        umesh%mesh%vertex(i,1,1)%y = def%xyzsp(2,i-inmin+1)
        if ( ndim == 3 ) &
        umesh%mesh%vertex(i,1,1)%z = def%xyzsp(3,i-inmin+1)
      enddo
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      do i = inmin, inmax
        umesh%mesh%vertex(i,1,1)%x = def%xyzdp(1,i-inmin+1)
        umesh%mesh%vertex(i,1,1)%y = def%xyzdp(2,i-inmin+1)
        if ( ndim == 3 ) &
        umesh%mesh%vertex(i,1,1)%z = def%xyzdp(3,i-inmin+1)
      enddo
    endselect

    ! Deallocate local xyz array
    select case(sctid)
    case(fluent_sct_spbinnodes)
      deallocate(def%xyzsp)
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      deallocate(def%xyzdp)
    endselect

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_nodes)
    strref = " "
  case(fluent_sct_spbinnodes, &
       fluent_sct_dpbinnodes)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

endsubroutine fluent_get_nodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : cells
!------------------------------------------------------------------------------!
subroutine fluent_get_cells(def, sctid, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer*4, allocatable :: celltype(:)
  integer           :: zoneid
  integer           :: icmin, icmax
  integer           :: ctype
  integer           :: itype
  integer           :: ityperef
  character(len=64) :: strzone
  character(len=64) :: strtype
  character(len=64) :: str, strref
  integer           :: i, inb
  integer           :: indx
  integer(kfp)      :: ilist(5)
  integer :: ierr
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str)

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = 0
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  icmin  = ilist(2)
  icmax  = ilist(3)
  itype  = ilist(4)
  ctype  = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
    write(strtype,'()')
  else
    write(strzone,'(a5,z4)') "zone ",zoneid
    if ( ctype == fluent_cells_mixed ) then
      write(str,'(a)') "Mixed"
    else
      write(str,'(a)') name_element(fluent2typhon_elemtype(ctype))
    endif
    write(strtype,'(2a)') " of type ",trim(str)
  endif
  write(str,'(3a,2(i8,a))') "(",trim(strzone),"): (",icmin,":",icmax,")"
  call cfd_print(" - reading FLUENT cells "//trim(str)//trim(strtype))

  ! Check itype
  if ( zoneid == 0 ) then
    ityperef = 0
  else
    ityperef = 1
  endif
  if ( itype /= ityperef ) then
    call cfd_warning("(fluent) type = "//trim(strof(itype)) &
                        //" /= typeref = "//trim(strof(ityperef)))
  endif

  ! Check ctype
  ! will be checked in celltype array

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then


  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then


    ! Allocate local celltype array
    ! WARNING : Could be used (globally) for checking
    allocate(celltype(icmin:icmax))

  ! If mixed cells then a list follows
  if ( ctype == fluent_cells_mixed ) then

    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

    ! Read cells
    select case(sctid)
    ! Ascii values
    case(fluent_sct_cells)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)
      indx = icmin
      do while ( indx < icmax )
        call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
        call str_read_ints(str, '(i)', inb, celltype(indx+1:icmax), ierr)
        if ( ierr /= 0 ) then
          call cfd_error("(fluent) could not read integer list")
        endif
        indx = indx + inb
      enddo
    ! Binary values (simple precision)
    case(fluent_sct_spbincells)
      read(def%iu_bin) (celltype(i),i=icmin,icmax)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')
  endif

    ! Transfer cells

    ! Deallocate local celltype array
    ! WARNING : Could be used (globally) for checking
    deallocate(celltype)

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_cells)
    strref = " "
  case(fluent_sct_spbincells)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

endsubroutine fluent_get_cells

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : faces
!------------------------------------------------------------------------------!
subroutine fluent_get_faces(def, sctid, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer           :: zoneid
  integer           :: ifmin, ifmax
  integer           :: bctype
  integer           :: bctyperef
  integer           :: ftype
  integer           :: ftyperef
  character(len=64) :: strzone
  character(len=64) :: str, strref
  integer,parameter :: nstrl = 256
  character(len=64) :: strl(1:nstrl)
  integer           :: nstrm, j
  integer(kfp)      :: inttab(64)
  integer           :: i, inb
  integer           :: tmpa, tmpb
  integer(kfp)      :: ilist(5)
  integer           :: nfvtex, cell1, cell2
  integer           :: lvtex(64)
  integer :: ierr
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str)

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = 0
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  ifmin  = ilist(2)
  ifmax  = ilist(3)
  bctype = ilist(4)
  ftype  = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
  else
    write(strzone,'(a5,z4)') "zone ",zoneid
  endif
  write(str,'(3a,2(i8,a))') "(",trim(strzone),"): (",ifmin,":",ifmax,")"
  write(str,'(a,2(a,z2.2))') trim(str)," bctype=",bctype," ftype=",ftype
  call cfd_print(" - reading FLUENT faces "//trim(str))

  ! Check bctype
  if ( zoneid == 0 ) then
    bctyperef = 0
  else
    bctyperef = bctype
  endif
  if ( bctype /= bctyperef ) then
    call cfd_warning("(fluent) bctype = "//trim(strof(bctype)) &
                        //" /= bctyperef = "//trim(strof(bctyperef)))
  endif

  ! Check ftype
  if ( zoneid == 0 ) then
    ftyperef = 0
  else
    ftyperef = ftype
  endif
  if ( ftype /= ftyperef ) then
    call cfd_warning("(fluent) ftype = "//trim(strof(ftype)) &
                        //" /= ftyperef = "//trim(strof(ftyperef)))
  endif

  if ( ftype == fluent_faces_polyg ) then
    call cfd_warning("(fluent) polygonal faces read procedure was not checked")
  endif

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then


  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then

  !if ( ftype == fluent_faces_mixed ) then
    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

    call cfd_warning("Faces are read but NOT STORED !!!")
    ! Read faces
    select case(sctid)
    ! Ascii values
    case(fluent_sct_faces)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)

      !!! ALL THIS IS TO READ FASTER CHAR BY CHAR...
      tmpa = ifmin - 1 + nstrl*16
      tmpb = ifmin - 1 + nstrl*16*64
      i = ifmin-1
      do while ( i < ifmax )
        if ( i+nstrl > ifmax ) then
          nstrm = ifmax - i
        else
          nstrm = nstrl
        endif
        do j = 1,nstrm
          call fll_storeuptochar(def%iu_bin, def%c, nl_char, strl(j))
        enddo
        if ( i >= tmpa ) then
          write(6,'(a,$)') "." ; call flush(6)
          tmpa = tmpa + nstrl*16
        endif
        if ( i >= tmpb ) then
          write(6,'()') ; call flush(6)
          tmpb = tmpb + nstrl*1024
        endif
        do j = 1,nstrm
        call str_read_ints(strl(j), '(z)', inb, inttab, ierr)
        if ( ierr /= 0 ) then
          call cfd_error("(fluent) could not read integer list")
        endif
        enddo
        i = i + nstrm
      stropt = " "
      indopt = 1
      enddo
      if ( tmpa > tmpb ) then
        write(6,'()') ; call flush(6)
      endif
      !!!... YES UP TO HERE

      !do i = ifmin,ifmax
      !  call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
      !  call str_read_ints(str, '(z)', inb, inttab, ierr)
      !  if ( ierr /= 0 ) then
      !    call cfd_error("(fluent) could not read integer list")
      !  endif
      !stropt = " "
      !indopt = 1
      !enddo

    ! Binary values (simple precision)
    case(fluent_sct_spbinfaces)
      if ( ftype == fluent_faces_mixed &
      .or. ftype == fluent_faces_polyg ) then
        read(def%iu_bin) &
          ((nfvtex, &
            (lvtex(j),j=1,nfvtex), &
            cell1,cell2),i=ifmin,ifmax)
      else
        read(def%iu_bin) &
          (((lvtex(j),j=1,ftype), &
            cell1,cell2),i=ifmin,ifmax)
      endif
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')
  !endif

    ! Transfer faces

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_faces)
    strref = " "
  case(fluent_sct_spbinfaces)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
    ! "Section" may be followed by two spaces...
    ! (for "2013" in tested msh files)
    if ( str(22:23) == "  " ) then
      strref(23:) = strref(22:)
    endif
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

endsubroutine fluent_get_faces

!------------------------------------------------------------------------------!
!> @brief FLUENT : read zone boundary and bcs
!------------------------------------------------------------------------------!
subroutine fluent_get_zonebcs(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call fluent_read_strlist(def, str)
  call fluent_read_strlist(def, str)

endsubroutine fluent_get_zonebcs

!------------------------------------------------------------------------------!
!> @brief FLUENT : read zone boundary
!------------------------------------------------------------------------------!
subroutine fluent_get_zonebnd(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call fluent_read_strlist(def, str)

endsubroutine fluent_get_zonebnd

!------------------------------------------------------------------------------!
!> @brief FLUENT : ignored section
!------------------------------------------------------------------------------!
subroutine fluent_get_ignored(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call fluent_read_strlist(def, str)
  write(6,'(3a)') "(ignored   : '",trim(str),"')"

endsubroutine fluent_get_ignored

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section end (trailing ')')
!------------------------------------------------------------------------------!
subroutine fluent_get_sctend(def, ieof)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  logical            , intent(out)   :: ieof
  ! -- Internal variables --
  integer :: ierr
  ! -- BODY --
  call affstar("get_s_end", def%c)
  call fll_checknextchar(def%iu_bin, def%c, ')')
  read(def%iu_bin,iostat=ierr) def%c
  ieof = ( ierr /= 0 )
  if ( ieof ) then
    write(6,'(a)') "(fluent) End of file..." ; call flush(6)
  endif
  call affexit("get_s_end", def%c)

endsubroutine fluent_get_sctend

endmodule FLUENT
