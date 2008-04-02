!------------------------------------------------------------------------------!
! F90 MODULE : ELEMVTEX
!
! Structures, constants and routines for MESH ELEMENT CONNECTIVITY
!------------------------------------------------------------------------------!
module ELEMVTEX

use TYPHMAKE
use OUTPUT

!------------------------------------------------------------------------------!
implicit none

!------------------------------------------------------------------------------!
! CONSTANTS for ELEMENT TYPE
!------------------------------------------------------------------------------!
integer(kpp), parameter :: elem_bar2   = 1
integer(kpp), parameter :: elem_tri3   = 2
integer(kpp), parameter :: elem_quad4  = 3
integer(kpp), parameter :: elem_ngon   = 4
integer(kpp), parameter :: elem_tetra4 = 11
integer(kpp), parameter :: elem_pyra5  = 12
integer(kpp), parameter :: elem_penta6 = 13
integer(kpp), parameter :: elem_hexa8  = 14


!------------------------------------------------------------------------------!
! TYPES/STRUCTURES
!------------------------------------------------------------------------------!
type st_elemvtex
  integer(kpp)          :: elemtype       ! type of element (cf constants)
  integer(kip)          :: nelem, nvtex   ! number of elements, number of vtex per element
  integer(kip), pointer :: ielem(:)       ! element global index
  integer(kip), pointer :: elemvtex(:,:)  ! element to vtex connectivity
endtype st_elemvtex

type st_genelemvtex
  integer(kpp)               :: ntype          ! number of allocated types
  type(st_elemvtex), pointer :: elem(:)        ! element connectivy
endtype st_genelemvtex

!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! Function : compute number of VTEX in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function nvtex_element(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_bar2)
  nvtex_element = 2
case(elem_tri3)
  nvtex_element = 3
case(elem_quad4)
  nvtex_element = 4
case(elem_ngon)
  nvtex_element = 0
case(elem_tetra4)
  nvtex_element = 4
case(elem_pyra5)
  nvtex_element = 5
case(elem_penta6)
  nvtex_element = 6
case(elem_hexa8)
  nvtex_element = 8
case default
  nvtex_element = -1
endselect

endfunction nvtex_element

!------------------------------------------------------------------------------!
! Constructor NEW elemvtex
!------------------------------------------------------------------------------!
subroutine new_elemvtex(elemvtex, nelem, itype, nvtex_opt)
implicit none
! -- dummy arguments --
type(st_elemvtex), intent(out) :: elemvtex
integer(kip),      intent(in)  :: nelem
integer(kpp),      intent(in)  :: itype
integer(kip), optional         :: nvtex_opt
! -- internal variables --

elemvtex%elemtype = itype
elemvtex%nelem    = nelem
elemvtex%nvtex    = nvtex_element(itype)

if (elemvtex%nvtex < 0) then
  call erreur("Element connectivity", "unknown element type")
endif

if (present(nvtex_opt).and.(elemvtex%nvtex == 0)) then
  elemvtex%nvtex = nvtex_opt
else
  call print_warning("number of vertices should not be forced")
endif

allocate(elemvtex%ielem   (nelem))
allocate(elemvtex%elemvtex(nelem, elemvtex%nvtex))

endsubroutine new_elemvtex

!------------------------------------------------------------------------------!
! Destructor DELETE elemvtex
!------------------------------------------------------------------------------!
subroutine delete_elemvtex(elemvtex)
implicit none
! -- dummy arguments --
type(st_elemvtex), intent(out) :: elemvtex

elemvtex%elemtype = 0
elemvtex%nelem    = 0
elemvtex%nvtex    = 0

deallocate(elemvtex%ielem)
deallocate(elemvtex%elemvtex)

endsubroutine delete_elemvtex

!------------------------------------------------------------------------------!
! Constructor NEW genelemvtex
!------------------------------------------------------------------------------!
subroutine new_genelemvtex(genelemvtex, ntype)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(out) :: genelemvtex
integer(kpp),         intent(in)  :: ntype

! -- internal variables --

genelemvtex%ntype = ntype
if (ntype > 0) then
  allocate(genelemvtex%elem(ntype))
endif

endsubroutine new_genelemvtex

!------------------------------------------------------------------------------!
! Destructor DELETE genelemvtex
!------------------------------------------------------------------------------!
subroutine delete_genelemvtex(genelemvtex)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(out) :: genelemvtex

! -- internal variables --
integer :: i

if (genelemvtex%ntype >= 1) then
  do i = 1, genelemvtex%ntype
    call delete_elemvtex(genelemvtex%elem(i))
  enddo
  deallocate(genelemvtex%elem)
endif
genelemvtex%ntype = 0

endsubroutine delete_genelemvtex

!------------------------------------------------------------------------------!
! subroutine ADD elemvtex to genelemvtex
!------------------------------------------------------------------------------!
subroutine addelem_genelemvtex(genelemvtex)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(inout) :: genelemvtex

! -- internal variables --
type(st_elemvtex), pointer :: tab_elem(:)
integer                    :: dim

dim = genelemvtex%ntype

allocate(tab_elem(dim+1))

if (dim >= 1) then                             ! if existing ELEMVTEX sections
  tab_elem(1:dim) = genelemvtex%elem(1:dim)
endif

genelemvtex%elem  => tab_elem
genelemvtex%ntype =  dim + 1

endsubroutine addelem_genelemvtex

!------------------------------------------------------------------------------!
! subroutine GET index of type in genelemvtex
!------------------------------------------------------------------------------!
subroutine getindex_genelemvtex(genelemvtex, itype, index)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(in)  :: genelemvtex
integer,              intent(in)  :: itype
integer,              intent(out) :: index

! -- internal variables --
integer :: i 

index = 1
do while (index <= genelemvtex%ntype)
  if (genelemvtex%elem(index)%elemtype == itype) exit
  index = index + 1
enddo
if (index > genelemvtex%ntype) index = 0

endsubroutine getindex_genelemvtex


!------------------------------------------------------------------------------!
endmodule
!------------------------------------------------------------------------------!
! History
! Apr  2008 : creation, element/vtex connectivity (derived from USTMESH)
!------------------------------------------------------------------------------!

