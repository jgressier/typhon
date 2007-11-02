!------------------------------------------------------------------------------!
! MODULE : FCT_PARSER                     Auteur : J. Gressier
!                                         Date   : July 2004
! 
! Create FCT_NODE tree from string
!
!
!------------------------------------------------------------------------------!

module FCT_PARSER

use FCT_DEF
use FCT_NODE
!use FCT_FUNC
use STRING

implicit none
     
! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------

interface convert_to_funct
  module procedure real4_to_funct, real8_to_funct, string_to_funct
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! convert STRING to FCT_FUNCT structure
!------------------------------------------------------------------------------!
subroutine string_to_funct(str, func, ierr)
implicit none

! -- parameters --
character(len=*),  intent(in)  :: str         ! string   input
type(st_fct_node), intent(out) :: func        ! function output
integer,           intent(out) :: ierr        ! error code

! -- internal variables --
character(len=len(str)) :: strpack    ! intermediate string

! -- body --

strpack = delspace(str)
strpack = lowercase(strpack)
call string_to_node(trim(strpack), func)

ierr = 0

endsubroutine string_to_funct


!------------------------------------------------------------------------------!
! convert REAL CONSTANT to FCT_FUNCT structure
!------------------------------------------------------------------------------!
subroutine real4_to_funct(x, func, ierr)
implicit none

! -- parameters --
real(4),           intent(in)  :: x
type(st_fct_node), intent(out) :: func        ! function output
integer,           intent(out) :: ierr        ! error code

! -- body --

call new_fct_node(func, node_cst, "")
func%container%r = x

ierr = 0

endsubroutine real4_to_funct



!------------------------------------------------------------------------------!
! convert REAL CONSTANT to FCT_FUNCT structure
!------------------------------------------------------------------------------!
subroutine real8_to_funct(x, func, ierr)
implicit none

! -- parameters --
real(8),           intent(in)  :: x
type(st_fct_node), intent(out) :: func        ! function output
integer,           intent(out) :: ierr        ! error code

! -- body --

call new_fct_node(func, node_cst, "")
func%container%r = x

ierr = 0

endsubroutine real8_to_funct


!------------------------------------------------------------------------------!
! convert STRING to FCT_NODE structure
!   - spaces are supposed to be removed
!   - operator are matched with only small letters
!------------------------------------------------------------------------------!
recursive subroutine string_to_node(str, node)
implicit none

! -- parameters --
character(len=*),  intent(in)  :: str
type(st_fct_node), intent(out) :: node

! -- internal variables --
integer(ipar) :: iop             ! operator or function index
integer       :: istr, lstr      ! index of operator in string, length of string
integer       :: pos, endpos
integer       :: ierr            ! error code
real(rprc)    :: x               ! temporary real
logical       :: found

! -- body --

found = .false.                 ! skip all seeking process once found something
lstr  = len(trim(str))

!-------------------------------------------------------
! remove brackets enclosing the whole string

if (str(1:1) == '(') then
  istr = index_oper(str(2:lstr), ")")
  if (istr == 0) then           ! ------ ")" not found
    call set_fct_error(-1, "unmatched ')' in the processed string")
  elseif (istr == lstr-1) then  ! ------ end of processed string
    call string_to_node(str(2:lstr-1), node)
    found = .true.
  else
    ! nothing to do if ending bracket inside the string
  endif
endif

!-------------------------------------------------------
! look for constants
!print*,found,trim(str),is_real(str)
if ((.not.found).and.(is_real(str))) then
  read(str, *, iostat=ierr) x
  if (ierr == 0) then
    call new_fct_node(node, node_cst, "")
    node%container%r = x
    found = .true.
  endif
endif

!-------------------------------------------------------
! look for binary operator

iop = min_op

do while ((.not.found).and.(iop <= max_op))  ! ---------- loop on binary operators

  !-------------------------------------------------------
  ! look for LAST binary operator in string 
  ! this is to handle unexpected effect of non-symmetric operators : - and /

  istr   = 0
  pos    = lstr        ! force first loop
  do while (pos /= 0)
    pos = index_oper(str(istr+1:lstr), trim(op2name(iop)))    ! FIRST index of operator
    istr = istr+pos
  enddo

  !-------------------------------------------------------
  ! operator (iop) not found -> next operator
  if (istr == 0) then 
    iop = iop + 1 

  !-------------------------------------------------------
  ! operator (iop) found but at end of string
  elseif (istr == lstr) then  
    call set_fct_error(-1, "unexpected ending position of binary operator")

  !-------------------------------------------------------
  ! operator (iop) found but at position 1
  elseif (istr == 1)  then
    
    select case(iop)
    case(op_add) ! -- the beginning "+" has no useful meaning --
      call string_to_node(str(2:lstr), node)
      found = .true.
    case(op_sub) ! -- the beginning "-" is converted to unary "opposite" operator --
      call new_fct_node(node, node_opunit, "", fct_opp)
      call string_to_node(str(2:lstr), node%left)
      found = .true.
    case default
      call set_fct_error(-1, "unexpected beginning position of binary operator")
    endselect

  elseif ((istr >= 2).and.(istr <= lstr-1)) then ! --------- found at consistent position

    ! -- if found, create node, split string and parse substrings
    call new_fct_node(node, node_opbin, "", iop)
    call string_to_node(str(1:istr-1),                          node%left)
    call string_to_node(str(istr+len(trim(op2name(iop))):lstr), node%right)
    found = .true.

  else
    call set_fct_error(istr, "(string_to_node) internal error")
  endif

enddo

!-------------------------------------------------------
! look for unary operator 
! supposed to be no binary operator yet 
! expected "oper(....)" - opposite operator not included in search

iop = min_fct

do while ((.not.found).and.(iop <= max_fct))      ! ---------- loop on unary operators

  !istr = index_oper(str, trim(op1name(iop))//"(")      ! index of "operator("
  istr = index(str, trim(op1name(iop))//"(")      ! index of "operator("

  if (istr == 0) then         ! --------- not found
    iop = iop + 1             !           -> next operator

  elseif (istr == 1) then     ! --------- found but check that it is at position 1
    ! -- if found, create node, delete operator in string and parse substrings
    call new_fct_node(node, node_opunit, "", iop)
    call string_to_node(str(len(trim(op1name(iop)))+1:lstr), node%left)
    found = .true.

  elseif ((istr >= 2).and.(istr <= lstr)) then  ! --------- found but unexpected position
    !call set_fct_error(-1, "unexpected position of unary operator") 
    ! it often means that current operator is part of another name : no error at the moment
    iop = iop + 1
  else
    call set_fct_error(istr, "(string_to_node) internal error")
  endif

enddo

!-------------------------------------------------------
! then : variables
! DEV : should add some consistency checks of variable names

if (.not.found) then
  if (index("abcdefghiklmnopkrstuvwxyz_", str(1:1)) == 0) then    ! check first letter
    call set_fct_error(istr, "(string_to_node) internal error : bad variable name")
  else
    call new_fct_node(node, node_var, str)
  endif
endif

endsubroutine string_to_node


!------------------------------------------------------------------------------!
! function FIRST index of a operator string into a string & ignore inside brackets
!   - spaces are supposed to be removed
!   - can look for "(", ")", unary or binary operators
!------------------------------------------------------------------------------!
recursive function index_oper(str, strop) result (ind)
implicit none

! -- parameters --
character(len=*)  :: str          ! string to look in
character(len=*)  :: strop        ! string to look for
integer           :: ind          ! result index

! -- internal variables --
integer :: lstr, lstrop           ! string lengths
integer :: iop, ibk1, ibk2        ! absolute index of operator and opening bracket

! -- body --

ind  = 0
lstr   = len(str)          ! length of string
lstrop = len(strop)        ! length of operator string

iop  = index(str, trim(strop))    ! index of "operator"      in full string
ibk1 = index(str, "(")            ! index of opening bracket in full string
if (ibk1 == 0) ibk1 = lstr+1      ! if no '(', trick to avoid ibk1 effect

if (iop == ibk1) then     ! -------- oper = '(' -----------------
  ind = iop               ! looking for '(' and found

elseif (iop < ibk1) then  ! -------- manage oper first ----------
  ind = iop               ! looking for operator and found

elseif (iop > ibk1) then  ! -------- manage '('  first ----------

  ibk2 = ibk1 + index_oper(str(ibk1+1:lstr), ")")      ! looking for ending bracket
  if (ibk1 == ibk2) call set_fct_error(ibk1, "did not find ending bracket")

  ! recursive search of "oper" after ending bracket
  ind  = ibk2 + index_oper(str(ibk2+1:lstr), strop)    ! look for oper after brackets
  if (ind == ibk2) ind = 0                             ! if not found, ind is set to 0

endif

endfunction index_oper


!------------------------------------------------------------------------------!
endmodule FCT_PARSER
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2006 : module creation
!------------------------------------------------------------------------------!
