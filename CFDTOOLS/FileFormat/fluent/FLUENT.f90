!------------------------------------------------------------------------------!
!> @brief FLUENT module, definition, reading routines
!! FLUENT format is detailed on
!! http://aerojet.engr.ucdavis.edu/fluenthelp/html/ug/node1490.htm
!------------------------------------------------------------------------------!
! f=readmsh ; m=cone.msh ; ifort -o $f $f.f90 && ./$f <<< $m
module FLUENT

use IO_UNIT
use IOCFD

implicit none

! -- Global Variables -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

integer, parameter :: kpf = 8

character(len=262144):: stropt
character(len=4096) :: namoptlist
character(len=64)   :: namopt
integer             :: indopt
integer             :: hedopt
integer,parameter   :: lenopt   = 10
integer             :: incopt
integer,parameter   :: lennames = 30

integer :: espion

!------------------------------------------------------------------------------!
! CONSTANTS for FLUENT ELEMENT TYPE
!------------------------------------------------------------------------------!

! Section headers
integer(kpf), parameter :: fluent_sct_comment   =  0
integer(kpf), parameter :: fluent_sct_header    =  1
integer(kpf), parameter :: fluent_sct_dim       =  2
integer(kpf), parameter :: fluent_sct_nodes     = 10
integer(kpf), parameter :: fluent_sct_spbinnodes = 2010
integer(kpf), parameter :: fluent_sct_dpbinnodes = 3010
integer(kpf), parameter :: fluent_sct_cells     = 12
integer(kpf), parameter :: fluent_sct_bincells  = 2012
integer(kpf), parameter :: fluent_sct_faces     = 13
integer(kpf), parameter :: fluent_sct_binfaces  = 2013
integer(kpf), parameter :: fluent_sct_per       = 18
integer(kpf), parameter :: fluent_sct_zone      = 39
integer(kpf), parameter :: fluent_sct_zoneb     = 45
integer(kpf), parameter :: fluent_sct_ctree     = 58
integer(kpf), parameter :: fluent_sct_ftree     = 59
integer(kpf), parameter :: fluent_sct_fparent   = 61

! Ignored section headers
integer(kpf), parameter :: fluent_sct_ignore1   =  4

! Cell types
integer(kpf), parameter :: fluent_cells_mixed   =  0
integer(kpf), parameter :: fluent_cells_tri     =  1
integer(kpf), parameter :: fluent_cells_tetra   =  2
integer(kpf), parameter :: fluent_cells_quad    =  3
integer(kpf), parameter :: fluent_cells_hexa    =  4
integer(kpf), parameter :: fluent_cells_pyram   =  5
integer(kpf), parameter :: fluent_cells_wedge   =  6
integer(kpf), parameter :: fluent_cells_polyh   =  7

! BC types
integer(kpf), parameter :: fluent_bc_interior   =  2
integer(kpf), parameter :: fluent_bc_wall       =  3
integer(kpf), parameter :: fluent_bc_p_inlet    =  4
integer(kpf), parameter :: fluent_bc_p_outlet   =  5
integer(kpf), parameter :: fluent_bc_symmetry   =  7
integer(kpf), parameter :: fluent_bc_per_sdow   =  8
integer(kpf), parameter :: fluent_bc_p_farfld   =  9
integer(kpf), parameter :: fluent_bc_v_inlet    = 10
integer(kpf), parameter :: fluent_bc_periodic   = 12
integer(kpf), parameter :: fluent_bc_fan        = 14
integer(kpf), parameter :: fluent_bc_mflow_in   = 20
integer(kpf), parameter :: fluent_bc_interfce   = 24
integer(kpf), parameter :: fluent_bc_parent     = 31
integer(kpf), parameter :: fluent_bc_outflow    = 36
integer(kpf), parameter :: fluent_bc_axis       = 37

! Face types
integer(kpf), parameter :: fluent_faces_mixed   =  0
integer(kpf), parameter :: fluent_faces_linear  =  2
integer(kpf), parameter :: fluent_faces_tri     =  3
integer(kpf), parameter :: fluent_faces_quad    =  4
integer(kpf), parameter :: fluent_faces_polyg   =  5

! -- Private data --

! -- Parameters --

interface str_to_int
  module procedure str_to_intkpf
endinterface

! -- Data types --

!------------------------------------------------------------------------------!
! ST_DEFFLUENT
!------------------------------------------------------------------------------!
type st_deffluent
  integer      :: version
  integer      :: iu_form, iu_bin
  logical      :: binary
  integer      :: nnodes, nelem, nbc
  character(len=1) :: c                 ! one character read in advance
  character(len=lennames), allocatable :: bcnames(:) 
end type st_deffluent


!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
!> @brief open FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_openread(filename, deffluent)
implicit none
! -- INPUTS --
integer            :: iunit
character(len=*)   :: filename
! -- OUTPUTS --
type(st_deffluent) :: deffluent
! -- private data --
integer :: info
! -- BODY --

deffluent%iu_form = getnew_io_unit()
OPEN(deffluent%iu_form, FILE=filename, ACCESS="STREAM", FORM="FORMATTED", iostat = info)
if (info /= 0) call cfd_error("fluent: unable to open formatted file "//trim(filename))

deffluent%iu_bin = getnew_io_unit()
OPEN(deffluent%iu_bin, FILE=filename, ACCESS="STREAM", FORM="UNFORMATTED", CONVERT='LITTLE_ENDIAN', iostat = info)
if (info /= 0) call cfd_error("fluent: unable to open unformatted file "//trim(filename))

read(deffluent%iu_bin) deffluent%c

end subroutine fluent_openread

!------------------------------------------------------------------------------!
!> @brief close FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_close(deffluent)
implicit none
! -- INPUTS --
type(st_deffluent) :: deffluent
! -- OUTPUTS --
! -- private data --
integer :: info
! -- BODY --

call close_io_unit(deffluent%iu_form)
call close_io_unit(deffluent%iu_bin)

end subroutine fluent_close


!------------------------------------------------------------------------------!
subroutine dumpp()
implicit none
call flush(6)
write(6,'(a)') trim(stropt)
call flush(6)
endsubroutine dumpp

!------------------------------------------------------------------------------!
subroutine stopp()
implicit none
integer :: i
character(len=1) :: c
do i = 1,16
  read(12) c
  call affline("follow: c='"//chnl(c)//"'")
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

subroutine affstar(optname, c)
implicit none
character(len=*) :: optname
character(len=1) :: c
hedopt = hedopt+1
write(namopt,'(a)') " "//optname
write(namoptlist(lenopt* hedopt+1 : &
                 lenopt*(hedopt+1)),'(a)') namopt(1:lenopt)
incopt = 1
call affline("(start: c='"//chnl(c)//"')")
incopt = 0
endsubroutine affstar

subroutine affexit(optname, c)
implicit none
character(len=*) :: optname
character(len=1) :: c
incopt = 1
call affline("(exit:  c='"//chnl(c)//"')")
incopt = 0
hedopt = hedopt-1
write(namopt,'(a)') namoptlist(lenopt* hedopt+1 : &
                               lenopt*(hedopt+1))
endsubroutine affexit

subroutine affline(str)
implicit none
character(len=*) :: str
character(len=1024), parameter :: blkstr = " "
write(stropt(indopt:len(stropt)),'(7a)') &
  blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
  blkstr(1:2*hedopt),trim(str),achar(10)
indopt = indopt + len(str) + lenopt + 3 + 4*hedopt + 1
endsubroutine affline

subroutine affhead(str)
implicit none
character(len=*) :: str
character(len=1024), parameter :: blkstr = " "
write(stropt(indopt:len(stropt)),'(6a)') &
  blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
  blkstr(1:2*hedopt),trim(str)
indopt = indopt + len(str) + lenopt + 3 + 4*hedopt
endsubroutine affhead

subroutine affcont(str)
implicit none
character(len=*) :: str
write(stropt(indopt:len(stropt)),'(a)') &
  trim(str)
indopt = indopt + len(str)
endsubroutine affcont

subroutine afftail(str)
implicit none
character(len=*) :: str
write(stropt(indopt:len(stropt)),'(2a)') &
  trim(str), achar(10)
indopt = indopt + len(str) + 1
endsubroutine afftail

subroutine str_index_spc(str, indspc, nspc)
implicit none
character(len=*) :: str
integer          :: indspc(*)
integer          :: nspc
integer          :: i, prevspc
nspc = 0
prevspc = 0
do i = 1,len_trim(str)
  if ( str(i:i) .eq. ' ' ) then
    if ( prevspc .eq. 0 ) then
      nspc = nspc+1
      indspc(nspc) = i
    endif
    prevspc = 1
  else
    prevspc = 0
  endif
enddo
if ( prevspc .eq. 0 ) then
  nspc=nspc+1
  indspc(nspc) = i-1
endif
endsubroutine str_index_spc

subroutine str_read_ints(str, ilist, inb, formt)
implicit none
character(len=*) :: str
integer(kpf)     :: ilist(*)
integer          :: inb
character(len=*) :: formt
integer          :: indspc(64), nspc
integer          :: ispc, ii
!call affstarp("str_read_ints")
!write(6,'(a)') "'"//trim(str)//"'"
call str_index_spc(str, indspc, nspc)
ii = 1
inb = 0
do ispc = 1, nspc
  inb = inb+1
  ilist(inb) = str_to_intkpf(str(ii:indspc(ispc)), formt)
!write(6,'(i4,$)') ilist(inb)
  ii =indspc(ispc)+1
enddo
!write(6,'()')
!call affexitp("str_read_ints")
endsubroutine str_read_ints

integer(kpf) function str_to_intkpf(str, formt)
implicit none
character(len=*) :: str
character(len=*) :: formt
integer(kpf) :: ivalue
integer :: ierr
call affstarp("str_to_intkpf")
call affline("str = '"//trim(str)//"'")
call affline("fmt = '"//trim(formt)//"'")
read(str,formt,iostat=ierr) ivalue
if ( ierr /= 0 ) then
  write(6,'(a)') "Error !"
  call stopp()
endif
str_to_intkpf = ivalue
call affexitp("str_to_intkpf")
endfunction str_to_intkpf

character(len=1) function chnl(c)
implicit none
character(len=1) :: c
chnl = c
if ( c .eq. achar(10) ) then
  chnl = "\"
endif
endfunction chnl

character(len=16) function chardesc(c)
implicit none
character(len=1) :: c
select case(c)
case(' ')
  chardesc = "space"
case(achar(10))
  chardesc = "nline"
case default
  chardesc = "char"
endselect
endfunction chardesc

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : skip blank (space or newline)
!------------------------------------------------------------------------------!
subroutine fll_ignore_blank(iu, c)
implicit none
integer           :: iu
character(len=1)  :: c
call affstar("ignore_b_", c)
do while ( c .eq. ' ' &
      .or. c .eq. achar(10) )
  call affline("'"//chnl(c)//"'"// &
               " "//trim(chardesc(c))//" skipped")
  read(iu) c
enddo
call affexit("ignore_b_", c)
endsubroutine fll_ignore_blank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store decimal digits
!------------------------------------------------------------------------------!
subroutine fll_storedecdigits(iu, str, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: c
integer :: i
call affstar("store_dec", c)
str = ""
i = 0
call affhead("'")
do while ( c .ge. '0' .and. c .le. '9' )
call affcont(c)
  i = i+1
  str(i:i) = c
  read(iu) c
enddo
call afftail("' stored")
call affexit("store_dec", c)
endsubroutine fll_storedecdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store hexadecimal digits
!------------------------------------------------------------------------------!
subroutine fll_storehexdigits(iu, str, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: c
integer :: i
call affstar("store_hex", c)
str = ""
i = 0
call affhead("'")
do while ( ( c .ge. '0' .and. c .le. '9' ) &
     .and. ( c .ge. 'a' .and. c .le. 'f' ) )
call affcont(c)
  i = i+1
  str(i:i) = c
  read(iu) c
enddo
call afftail("' stored")
call affexit("store_hex", c)
endsubroutine fll_storehexdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to separator (blank or bracket)
!------------------------------------------------------------------------------!
subroutine fll_storeuptosepbb(iu, str, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: c
integer :: i
call affstar("store_sep", c)
str = ""
i = 0
call affhead("'")
do while ( c .ne. ' ' &
     .and. c .ne. achar(10) &
     .and. c .ne. '(' &
     .and. c .ne. ')' )
call affcont(c)
  i = i+1
  str(i:i) = c
  read(iu) c
enddo
call affcont("' stored")
call afftail(" ; str='"//trim(str)//"'")
call affexit("store_sep", c)
endsubroutine fll_storeuptosepbb

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to blank (space or newline)
!------------------------------------------------------------------------------!
subroutine fll_storeuptoblank(iu, str, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: c
integer :: i
call affstar("store_nb_", c)
str = ""
i = 0
call affhead("'")
do while ( c .ne. ' ' &
     .and. c .ne. achar(10) )
call affcont(c)
  i = i+1
  str(i:i) = c
  read(iu) c
enddo
call afftail("' stored")
call affexit("store_nb_", c)
endsubroutine fll_storeuptoblank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_storeuptocharnostep(iu, str, cc, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: cc
character(len=1)  :: c
integer :: i
!call affstar("store_chr", c)
str = ""
i = 0
!call affhead("'")
do while ( c .ne. cc )
  i = i+1
  if ( i .gt. len(str) ) then
    call stopp()
  endif
  str(i:i) = c
  read(iu) c
enddo
!call afftail("' stored")
!call affline("'"//chnl(c)//"' checked up to")
endsubroutine fll_storeuptocharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!------------------------------------------------------------------------------!
subroutine fll_storeuptochar(iu, str, cc, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: cc
character(len=1)  :: c
call affstar("store_chr", c)
call fll_storeuptocharnostep(iu, str, cc, c)
read(iu) c
call affline("next : '"//c//"'")
call affexit("store_chr", c)
endsubroutine fll_storeuptochar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_checknextcharnostep(iu, cc, c)
implicit none
integer           :: iu
character(len=1)  :: cc
character(len=1)  :: c
integer :: i
call affstar("check_nxt", c)
call fll_ignore_blank(iu, c)
if ( c .ne. cc ) then
  call affline("cc = '"//cc//"'")
  call affline("c  = '"//c //"'")
  write(6,'(a)') "Error !"
  call stopp()
endif
call affline("'"//chnl(c)//"' checked")
call affexit("check_nxt", c)
endsubroutine fll_checknextcharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!------------------------------------------------------------------------------!
subroutine fll_checknextchar(iu, cc, c)
implicit none
integer           :: iu
character(len=1)  :: cc
character(len=1)  :: c
call affstar("check_nxt", c)
call fll_checknextcharnostep(iu, cc, c)
read(iu) c
call affexit("check_nxt", c)
endsubroutine fll_checknextchar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read integer
!------------------------------------------------------------------------------!
subroutine fll_getint(iu, val, c)
implicit none
integer           :: iu
integer           :: val
character(len=1)  :: c
character(len=64) :: str
call affstar("get_int__", c)
call fll_ignore_blank(iu, c)
call fll_storedecdigits(iu, str, c)
call affline("read integer from string : '"//trim(str)//"'")
val = str_to_int(str, '(i)')
call affline("integer read : "//trim(str))
call affexit("get_int__", c)
endsubroutine fll_getint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read hexadecimal integer
!------------------------------------------------------------------------------!
subroutine fll_gethexint(iu, val, c)
implicit none
integer           :: iu
integer           :: val
character(len=1)  :: c
character(len=64) :: str
call affstar("get_hex__", c)
call fll_ignore_blank(iu, c)
call fll_storeuptoblank(iu, str, c)
call affline("read integer from string : '"//trim(str)//"'")
val = str_to_int(str, '(z)')
write(str,'(i)') val
call affline("integer read : "//trim(str))
call affexit("get_hex__", c)
endsubroutine fll_gethexint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read string
!------------------------------------------------------------------------------!
subroutine fll_getstring(iu, str, c)
implicit none
integer           :: iu
character(len=*)  :: str
character(len=1)  :: c
call affstar("get_str__", c)
call fll_checknextchar(iu, '"', c)
call fll_storeuptochar(iu, str, '"', c)
call affline("read string : '"//trim(str)//"'")
call affexit("get_str__", c)
endsubroutine fll_getstring

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read list
!------------------------------------------------------------------------------!
subroutine fll_getlist(def, str)
implicit none
type(st_deffluent) :: def
character(len=*)  :: str
call affstar("get_list_", def%c)
call fll_checknextchar(def%iu_bin, '(', def%c)
call fll_storeuptochar(def%iu_bin, str, ')', def%c)
call affline("read list : '"//trim(str)//"'")
call affexit("get_list_", def%c)
endsubroutine fll_getlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : extract list
!------------------------------------------------------------------------------!
subroutine fll_extractlist(iu, strlistsize, strlist, c)
implicit none
integer           :: iu
integer           :: strlistsize
character(len=*)  :: strlist(strlistsize)
integer           :: nstr, inst, lstr
character(len=1)  :: c
character(len=1)  :: s
call affstar("extract__", c)
call fll_checknextchar(iu, '(', c)
do nstr = 1,strlistsize
  inst = 0
  strlist(nstr) = ""
  call fll_ignore_blank(iu, c)
  if ( c .eq. ')' ) then
    exit
  endif
  call fll_storeuptosepbb(iu, strlist(nstr), c)
write(s,'(i1)') nstr
call affline("strl["//s//"] = '"//trim(strlist(nstr))//"'")
  inst = len_trim(strlist(nstr))
enddo
nstr = nstr-1
write(6,'(a,i4,a,$)') "nstr =",nstr,"    "
strlistsize = nstr
call fll_checknextchar(iu, ')', c)
call affexit("extract__", c)
endsubroutine fll_extractlist

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section id (and leading '(')
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_id(def, sctid)
implicit none
type(st_deffluent) :: def
integer            :: sctid
character(len=64)  :: str
call affstar("get_s_id_", def%c)
call fll_checknextchar(def%iu_bin, '(', def%c)
call fll_storeuptoblank(def%iu_bin, str, def%c)
sctid = str_to_int(str, '(i)')
call affline("read section head : '"//trim(str)//"'")
call affexit("get_s_id_", def%c)
endsubroutine fluent_get_sct_id

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : comment
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_comment(def)
implicit none
type(st_deffluent) :: def
character(len=64)  :: str
call affstar("get_s_com", def%c)
call fll_getstring(def%iu_bin, str, def%c)
write(6,'(3a)') "Comment   : '",trim(str),"'"
call affexit("get_s_com", def%c)
endsubroutine fluent_get_sct_comment

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : header
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_header(def)
implicit none
type(st_deffluent) :: def
character(len=64) :: str
call affstar("get_s_hed", def%c)
call fll_getstring(def%iu_bin, str, def%c)
write(6,'(3a)') "Header    : '",trim(str),"'"
call affexit("get_s_hed", def%c)
endsubroutine fluent_get_sct_header

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : dimension
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_dim(iu, spacedim, c)
implicit none
integer           :: iu
integer           :: spacedim
character(len=1)  :: c
call affstar("get_s_dim", c)
call fll_getint(iu, spacedim, c)
write(6,'(a,i)') "Dimension : ",spacedim
call affexit("get_s_dim", c)
endsubroutine fluent_get_sct_dim

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : nodes
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_nodes(iu, sctid, zoneid, &
                                x, y, z, &
                                frstindx, lastindx, ntype, nd, ndim, c)
implicit none
integer           :: iu, sctid
integer           :: zoneid, frstindx, lastindx, ntype, nd, ndim
real*8            :: x(*),y(*),z(*)
integer           :: ntyperef
character(len=1)  :: c
character(len=64) :: lstr
character(len=64) :: str, strref
integer,parameter :: strsize=8
character(len=64) :: strlist(strsize)
integer           :: i, nstr, nstrref
call affstar("get_s_nod", c)
nstrref = 5
nstr = nstrref
call fll_extractlist(iu, nstr, strlist, c)
if ( nstr .ne. nstrref ) then
  write(str,'(a,i)') "exp nb of fields = ",nstrref ; call affline(trim(str))
  write(str,'(a,i)') "number of fields = ",nstr    ; call affline(trim(str))
  call affline("'"//trim(lstr)//"'")
  call stopp()
endif
write(6,'(5(1x,1H|,a,1H|),$)') (trim(strlist(i)),i=1,5)
read(strlist(1),'(z)') zoneid
read(strlist(2),'(z)') frstindx
read(strlist(3),'(z)') lastindx
read(strlist(4),'(i)') ntype
read(strlist(5),'(i)') nd
write(6,'(5(1H,,i8))') zoneid, frstindx, lastindx, ntype, nd
if ( zoneid .eq. 0 ) then
  ntyperef = 0
else
  ntyperef = 1
endif
if ( ntype .ne. ntyperef ) then
  write(str,'(a,i)') "ntyperef = ",ntyperef ; call affline(trim(str))
  write(str,'(a,i)') "ntype    = ",ntype    ; call affline(trim(str))
  write(6,'(a)') "Error !"
  call stopp()
endif
if ( ndim .ne. 0 ) then
if ( nd .ne. ndim ) then
  write(str,'(a,i)') "ndim = ",ndim ; call affline(trim(str))
  write(str,'(a,i)') "nd   = ",nd   ; call affline(trim(str))
  write(6,'(a)') "Error !"
  call stopp()
endif
endif
if ( zoneid .ne. 0 ) then
  call affline("'"//chnl(c)//"' in bag")
  !!!! fll_checknextcharnostep instead of fll_checknextchar
  call fll_checknextcharnostep(iu, '(', c)
  !!!! '(' is to be checked then leave next char unread
  select case(ndim)
  case(2)
    select case(sctid)
    case(fluent_sct_nodes)
      read(iu) c
      do i = frstindx,lastindx
      read(iu) c
      call fll_storeuptocharnostep(iu, str, achar(10), c)
  !write(6,'(a,$)') str(1:20)
  !call flush(6)
      read(str,*) x(i),y(i)
  !write(6,'(2e20.12)') x(i),y(i)
      enddo
    case(fluent_sct_spbinnodes)
      read(iu) (x(i),y(i),i=frstindx,lastindx)
    case(fluent_sct_dpbinnodes)
      read(iu) (x(i),y(i),i=frstindx,lastindx)
    endselect
  case(3)
    select case(sctid)
    case(fluent_sct_nodes)
      read(iu) c
      do i = frstindx,lastindx
      read(iu) c
      call fll_storeuptocharnostep(iu, str, achar(10), c)
  !write(6,'(a,$)') str(1:20)
  !call flush(6)
      read(str,*) x(i),y(i),z(i)
  !write(6,'(3e20.12)') x(i),y(i),z(i)
      enddo
    case(fluent_sct_spbinnodes)
      read(iu) (x(i),y(i),z(i),i=frstindx,lastindx)
    case(fluent_sct_dpbinnodes)
      read(iu) (x(i),y(i),z(i),i=frstindx,lastindx)
    endselect
  endselect
  !write(6,'(2z20)') x(frstindx),y(frstindx)
  !write(6,'(2z20)') x(lastindx),y(lastindx)
  !write(6,'(2e20.12)') x(frstindx),y(frstindx)
  !write(6,'(2e20.12)') x(lastindx),y(lastindx)
  call affline("(x,y) list read")
  !!!! next char was left unread
  read(iu) c
  !!!! it is read now
  call fll_checknextchar(iu, ')', c)
endif
call fll_ignore_blank(iu, c)
call fll_storeuptocharnostep(iu, str, ')', c)
select case(sctid)
case(fluent_sct_nodes)
  strref = " "
case(fluent_sct_spbinnodes, &
     fluent_sct_dpbinnodes)
  write(strref,'(a,i04)') "End of Binary Section ",sctid
endselect
if ( trim(str) .ne. trim(strref) ) then
  call affline("strref = '"//trim(strref)//"'")
  call affline("str    = '"//trim(str   )//"'")
  call stopp()
endif
call affexit("get_s_nod", c)
endsubroutine fluent_get_sct_nodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : cells
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_cells(iu, sctid, zoneid, &
                                celllist, &
                                frstindx, lastindx, ctype, c)
implicit none
integer           :: iu, sctid
integer           :: zoneid, frstindx, lastindx, ctype
integer           :: elemtype
integer(kpf)      :: celllist(*)
integer           :: ctyperef
character(len=1)  :: c
character(len=64) :: lstr
character(len=64) :: str, strref
integer,parameter :: strsize=8
character(len=64) :: strlist(strsize)
integer           :: i, nstr, nstrref, indx, inb
call affstar("get_s_cel", c)
nstrref = 5
nstr = nstrref
call fll_extractlist(iu, nstr, strlist, c)
if ( nstr .ne. nstrref ) then
  write(str,'(a,i)') "exp nb of fields = ",nstrref ; call affline(trim(str))
  write(str,'(a,i)') "number of fields = ",nstr    ; call affline(trim(str))
  call affline("'"//trim(lstr)//"'")
  call stopp()
endif
write(6,'(5(1x,1H|,a,1H|),$)') (trim(strlist(i)),i=1,5)
read(strlist(1),'(z)') zoneid
read(strlist(2),'(z)') frstindx
read(strlist(3),'(z)') lastindx
read(strlist(4),'(i)') ctype
read(strlist(5),'(i)') elemtype
write(6,'(5(1H,,i8))') zoneid, frstindx, lastindx, ctype, elemtype
if ( zoneid .eq. 0 ) then
  ctyperef = 0
else
  ctyperef = 1
endif
if ( ctype .ne. ctyperef ) then
  write(str,'(a,i)') "ctyperef = ",ctyperef ; call affline(trim(str))
  write(str,'(a,i)') "ctype    = ",ctype    ; call affline(trim(str))
  write(6,'(a)') "Error !"
  call stopp()
endif
if ( zoneid .ne. 0 ) then
if ( elemtype .eq. fluent_cells_mixed ) then
  call affline("'"//chnl(c)//"' in bag")
  !!!! fll_checknextcharnostep instead of fll_checknextchar
  call fll_checknextcharnostep(iu, '(', c)
  !!!! '(' is to be checked then leave next char unread
  select case(sctid)
  case(fluent_sct_cells)


    read(iu) c
    indx = 0
    do while ( indx .lt. lastindx )
      read(iu) c
      call fll_storeuptocharnostep(iu, str, achar(10), c)
write(6,'(a,$)') str(1:20)
call flush(6)
      call str_read_ints(str, celllist(indx+1:lastindx), inb, '(i)')
write(6,'(i)') celllist(indx+1:indx+inb)
      indx = indx + inb
    enddo


  case(fluent_sct_bincells)
    read(iu) (celllist(i),i=frstindx,lastindx)
  endselect
  call affline("elemtype list read")
  !!!! next char was left unread
  read(iu) c
  !!!! it is read now
  call fll_checknextchar(iu, ')', c)
endif
endif
call fll_ignore_blank(iu, c)
call fll_storeuptocharnostep(iu, str, ')', c)
select case(sctid)
case(fluent_sct_cells)
  strref = " "
case(fluent_sct_bincells)
  write(strref,'(a,i04)') "End of Binary Section ",sctid
endselect
if ( trim(str) .ne. trim(strref) ) then
  call affline("strref = '"//trim(strref)//"'")
  call affline("str    = '"//trim(str   )//"'")
  call stopp()
endif
call affexit("get_s_cel", c)
endsubroutine fluent_get_sct_cells

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : faces
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_faces(iu, sctid, zoneid, &
                                facelist, &
                                frstindx, lastindx, bctype, c)
implicit none
integer           :: iu, sctid
integer           :: zoneid, frstindx, lastindx, bctype
integer           :: facetype
integer(kpf)      :: facelist(*)
integer           :: bctyperef
integer           :: facetyperef
character(len=1)  :: c
character(len=64) :: lstr
character(len=64) :: str, strref
integer,parameter :: strsize=8
character(len=64) :: strlist(strsize)
integer(kpf)      :: inttab(64)
integer           :: inb
integer           :: i, nstr, nstrref
call affstar("get_s_fac", c)
nstrref = 5
nstr = nstrref
call fll_extractlist(iu, nstr, strlist, c)
if ( nstr .ne. nstrref ) then
  write(str,'(a,i)') "exp nb of fields = ",nstrref ; call affline(trim(str))
  write(str,'(a,i)') "number of fields = ",nstr    ; call affline(trim(str))
  call affline("'"//trim(lstr)//"'")
  call stopp()
endif
write(6,'(5(1x,1H|,a,1H|),$)') (trim(strlist(i)),i=1,5)
read(strlist(1),'(z)') zoneid
read(strlist(2),'(z)') frstindx
read(strlist(3),'(z)') lastindx
read(strlist(4),'(z)') bctype
read(strlist(5),'(i)') facetype
write(6,'(5(1H,,i8))') zoneid, frstindx, lastindx, bctype, facetype
if ( zoneid .eq. 0 ) then
  bctyperef = 0
  if ( bctype .ne. bctyperef ) then
    write(str,'(a,i)') "bctyperef = ",bctyperef ; call affline(trim(str))
    write(str,'(a,i)') "bctype    = ",bctype    ; call affline(trim(str))
    write(6,'(a)') "Error !"
    call stopp()
  endif
  facetyperef = 0
  if ( facetype .ne. facetyperef ) then
    write(str,'(a,i)') "facetyperef = ",facetyperef ; call affline(trim(str))
    write(str,'(a,i)') "facetype    = ",facetype    ; call affline(trim(str))
    write(6,'(a)') "Error !"
    call stopp()
  endif
endif
if ( zoneid .ne. 0 ) then
!if ( facetype .eq. fluent_faces_mixed ) then
  call affline("'"//chnl(c)//"' in bag")
  !!!! fll_checknextcharnostep instead of fll_checknextchar
  call fll_checknextcharnostep(iu, '(', c)
  !!!! '(' is to be checked then leave next char unread
  select case(sctid)
  case(fluent_sct_faces)
    read(iu) c
    do i = frstindx,lastindx
      read(iu) c
      call fll_storeuptocharnostep(iu, str, achar(10), c)
      call str_read_ints(str, inttab, inb, '(z)')
  stropt = " "
  indopt = 1
    enddo
    select case(facetype)
    case(fluent_faces_mixed, &
         fluent_faces_polyg)
      facelist(i) = inttab(1)
    case default
      facelist(i) = facetype
    endselect
  case(fluent_sct_binfaces)
    read(iu) (facelist(i),i=frstindx,lastindx)
  endselect
  call affline("facetype list read")
  !!!! next char was left unread
  read(iu) c
  !!!! it is read now
  call fll_checknextchar(iu, ')', c)
!endif
endif
call fll_ignore_blank(iu, c)
call fll_storeuptocharnostep(iu, str, ')', c)
select case(sctid)
case(fluent_sct_faces)
  strref = " "
case(fluent_sct_binfaces)
  write(strref,'(a,i04)') "End of Binary Section ",sctid
endselect
if ( trim(str) .ne. trim(strref) ) then
  call affline("strref = '"//trim(strref)//"'")
  call affline("str    = '"//trim(str   )//"'")
  call stopp()
endif
call affexit("get_s_fac", c)
endsubroutine fluent_get_sct_faces

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section end (trailing ')')
!------------------------------------------------------------------------------!
subroutine fluent_get_sct_end(iu, ierr, c)
implicit none
integer           :: iu
integer           :: sctval
character(len=1)  :: c
character(len=64) :: str
integer :: ierr
call affstar("get_s_end", c)
call fll_checknextchar(iu, ')', c)
read(iu,iostat=ierr) c
if ( ierr /= 0 ) then
  write(6,'(a)') "End of file..." ; call flush(6)
endif
call affexit("get_s_end", c)
endsubroutine fluent_get_sct_end

end module
