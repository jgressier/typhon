!------------------------------------------------------------------------------!
! MODULE : FCT_PARSER                         Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Definition of NODE for equation tree
!
!------------------------------------------------------------------------------!

module FCT_PARSER

use FCT_NODE
use FCT_FUNC
use STRING

implicit none


     
! -- DECLARATIONS -----------------------------------------------------------



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! convert STRING to FCT_FUNCT structure
!------------------------------------------------------------------------------!
subroutine string_to_funct(str, func)
implicit none

! -- parameters --
character(len=*)  :: str         ! string   input
type(st_fct_func) :: func        ! function output

! -- internal variables --
character(len=len(str)) :: strpack    ! intermediate string

! -- body --

strpack = delspace(str)
strpack = lowercase(strpack)

call string_to_node(strpack, func%function)


endsubroutine string_to_funct


!------------------------------------------------------------------------------!
! convert STRING to FCT_NODE structure
!   - spaces are supposed to be removed
!   - operator are matched with only small letters
!------------------------------------------------------------------------------!
recursive subroutine string_to_node(str, node)
implicit none

! -- parameters --
character(len=*)  :: str
type(st_fct_node) :: node

! -- internal variables --
integer(ipar) :: iop             ! operator or function index
integer       :: istr, lstr      ! index of operator in string, length of string
integer       :: ierr            ! error code
real(iprc)    :: x               ! temporary real
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
! look for binary operator

iop = min_op

do while ((.not.found).and.(iop <= max_op))  ! ---------- loop on binary operators

  istr = index_oper(str, trim(op2name(iop)))    ! index of operator

  if (istr == 0) then         ! --------- not found
    iop = iop + 1             !           -> next operator
  elseif ((istr == 1).or.(istr == lstr)) then    ! --------- found but at position 1 or end
    call set_fct_error(-1, "unexpected position of binary operator")
  elseif ((istr >= 2).and.(istr <= lstr-1)) then ! --------- found at consistent position
    ! -- if found, create node, split string and parse substrings
    call new_fct_node(node, node_opbin, iop)
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
! expected "oper(....)"

iop = min_fct

do while ((.not.found).and.(iop <= max_fct))      ! ---------- loop on binary operators

  istr = index_oper(str, trim(op1name(iop)))      ! index of operator

  if (istr == 0) then         ! --------- not found
    iop = iop + 1             !           -> next operator
  elseif (istr == 1) then     ! --------- found but check that it is at position 1
    ! -- if found, create node, delete operator in string and parse substrings
    call new_fct_node(node, node_opunit, iop)
    call string_to_node(str(len(trim(op1name(iop)))+1:lstr), node%left)
    found = .true.
  elseif ((istr >= 2).and.(istr <= lstr)) then  ! --------- found but unexpected position
    call set_fct_error(-1, "unexpected position of unary operator")
  else
    call set_fct_error(istr, "(string_to_node) internal error")
  endif

enddo

!-------------------------------------------------------
! look for constants

if (.not.found) then
  read(str, *, iostat=ierr) x
  if (ierr /= 0) then
    call new_fct_node(node, node_cst)
    node%container%r = x
    found = .true.
  endif
endif

!-------------------------------------------------------
! then : variables
! DEV : should add some consistency checks of variable names

if (.not.found) then
  call new_fct_node(node, node_var)
  node%container%name = str
endif

endsubroutine string_to_node


!------------------------------------------------------------------------------!
! function index of a operator string into a string & pass brackets
!   - spaces are supposed to be removed
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
lstr   = len(str)
lstrop = len(strop)

iop  = index(str, trim(strop))
ibk1 = index(str, "(")
if (ibk1 == 0) ibk1 = lstr+1      ! if no '(', avoid ibk1 effect

if (iop == ibk1) then     ! -------- oper = '(' -----------------
  ind = iop
elseif (iop < ibk1) then  ! -------- manage oper first ----------
  ind = iop
elseif (iop > ibk1) then  ! -------- manage '('  first ----------
  ibk2 = ibk1 + index_oper(str(ibk1+1:lstr), ")")      ! look for ending bracket
  if (ibk1 == ibk2) call set_fct_error(ibk1, "did not find ending bracket")
  ind  = ibk2 + index_oper(str(ibk2+1:lstr), strop)    ! look for oper after brackets
  if (ind == ibk2) ind = 0                             ! not found
endif
!print*,"  ind",ind

endfunction index_oper


!------------------------------------------------------------------------------!
endmodule FCT_PARSER
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2006 : module creation
!------------------------------------------------------------------------------!
