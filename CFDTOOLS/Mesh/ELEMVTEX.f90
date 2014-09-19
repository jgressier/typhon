!------------------------------------------------------------------------------!
! F90 MODULE : ELEMVTEX
!
! Structures, constants and routines for MESH ELEMENT CONNECTIVITY
!------------------------------------------------------------------------------!
module ELEMVTEX

use MESHPREC
use IOCFD

!------------------------------------------------------------------------------!
implicit none

!------------------------------------------------------------------------------!
! CONSTANTS for ELEMENT TYPE
!> @todo documentation of cell vtex normalized connectivity
!------------------------------------------------------------------------------!
integer(kpp), parameter :: elem_null    = 0
integer(kpp), parameter :: elem_node    = 1
integer(kpp), parameter :: elem_bar2    = 2
integer(kpp), parameter :: elem_bar3    = 3
integer(kpp), parameter :: elem_tri3    = 13
integer(kpp), parameter :: elem_tri6    = 16
integer(kpp), parameter :: elem_quad4   = 24
integer(kpp), parameter :: elem_quad8   = 28
integer(kpp), parameter :: elem_quad9   = 29
integer(kpp), parameter :: elem_ngon    = 50
integer(kpp), parameter :: elem_tetra4  = 101
integer(kpp), parameter :: elem_tetra10 = 105
integer(kpp), parameter :: elem_pyra5   = 121
integer(kpp), parameter :: elem_pyra14  = 125
integer(kpp), parameter :: elem_penta6  = 131
integer(kpp), parameter :: elem_penta15 = 135
integer(kpp), parameter :: elem_penta18 = 138
integer(kpp), parameter :: elem_hexa8   = 141
integer(kpp), parameter :: elem_hexa20  = 145
integer(kpp), parameter :: elem_hexa27  = 148
integer(kpp), parameter :: elem_nedron  = 200

integer(kpp), parameter :: elem_nlen = 8

!------------------------------------------------------------------------------!
! TYPES/STRUCTURES
!------------------------------------------------------------------------------!
type st_elemvtex
  integer(kpp)          :: elemtype       ! type of element (cf constants)
  integer(kip)          :: nelem, nvtex   ! number of elements, number of vtex per element
  integer(kip), pointer :: ielem(:)       ! element global index
  integer(kip), pointer :: elemvtex(:,:)  ! element to vtex connectivity (1:nelem, 1:nvtex)
endtype st_elemvtex

type st_genelemvtex
  integer(kpp)               :: nsection       ! number of allocated types
  type(st_elemvtex), pointer :: elem(:)        ! element connectivy
endtype st_genelemvtex

!------------------------------------------------------------------------------!
interface dim_element
  module procedure dim_element, dim_elemtype
endinterface
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
case(elem_null)   ; nvtex_element = 0
case(elem_bar2)   ; nvtex_element = 2
case(elem_tri3)   ; nvtex_element = 3
case(elem_quad4)  ; nvtex_element = 4
case(elem_ngon)   ; nvtex_element = 0
case(elem_tetra4) ; nvtex_element = 4
case(elem_pyra5)  ; nvtex_element = 5
case(elem_penta6) ; nvtex_element = 6
case(elem_hexa8)  ; nvtex_element = 8
case default      ; nvtex_element = -1
endselect

endfunction nvtex_element

!------------------------------------------------------------------------------!
! Function : compute number of VTEX in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
character(len=elem_nlen) function name_element(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_null)   ; name_element = 'None'
case(elem_bar2)   ; name_element = 'Bar2'
case(elem_tri3)   ; name_element = 'Tri3'
case(elem_quad4)  ; name_element = 'Quad4'
case(elem_ngon)   ; name_element = 'Polygon'
case(elem_tetra4) ; name_element = 'Tetra4'
case(elem_pyra5)  ; name_element = 'Pyra5'
case(elem_penta6) ; name_element = 'Penta6'
case(elem_hexa8)  ; name_element = 'Hexa8'
case default      ; name_element = 'Unknown'
endselect

endfunction name_element

!------------------------------------------------------------------------------!
! Function : compute number of FACES in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function nface_element(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_null)   ; nface_element = 0
case(elem_bar2)   ; nface_element = 2
case(elem_tri3)   ; nface_element = 3
case(elem_quad4)  ; nface_element = 4
case(elem_ngon)   ; nface_element = 0
case(elem_tetra4) ; nface_element = 4
case(elem_pyra5)  ; nface_element = 5
case(elem_penta6) ; nface_element = 5
case(elem_hexa8)  ; nface_element = 6
case default      ; nface_element = -1
endselect

endfunction nface_element

!------------------------------------------------------------------------------!
! Function : compute number of VTEX PER FACES in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function nvtexperface_element(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_null)   ; nvtexperface_element = 0
case(elem_bar2)   ; nvtexperface_element = 1
case(elem_tri3)   ; nvtexperface_element = 2
case(elem_quad4)  ; nvtexperface_element = 2
case(elem_ngon)   ; nvtexperface_element = 2
case(elem_tetra4) ; nvtexperface_element = 3
case(elem_pyra5)  ; nvtexperface_element = 4
case(elem_penta6) ; nvtexperface_element = 4
case(elem_hexa8)  ; nvtexperface_element = 4
case default      ; nvtexperface_element = -1
endselect

endfunction nvtexperface_element

!------------------------------------------------------------------------------!
! Function : compute number of FACES in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function dim_elemtype(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_node)   ; dim_elemtype = 0
case(elem_bar2)   ; dim_elemtype = 1
case(elem_tri3, &
     elem_quad4, &
     elem_ngon)   ; dim_elemtype = 2
case(elem_tetra4, &
     elem_pyra5, &
     elem_penta6, &
     elem_hexa8)  ; dim_elemtype = 3
case default      ; dim_elemtype = -1
endselect

endfunction dim_elemtype

!------------------------------------------------------------------------------!
! Function : compute number of FACES in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
integer function dim_element(element)
implicit none
! -- dummy arguments --
type(st_elemvtex),  intent(in)  :: element

dim_element = dim_elemtype(element%elemtype)

endfunction dim_element

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
  call cfd_error("unknown element type in Element connectivity")
endif

if (present(nvtex_opt)) then
  if (elemvtex%nvtex == 0) then             ! nvtex not set
    elemvtex%nvtex = nvtex_opt
  elseif (elemvtex%nvtex == nvtex_opt) then ! nothing to do ! no warning
  else
    call cfd_warning("number of vertices should not be forced !!!")
    elemvtex%nvtex = nvtex_opt
  endif
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

elemvtex%elemtype = elem_null
elemvtex%nelem    = 0
elemvtex%nvtex    = 0

deallocate(elemvtex%ielem)
deallocate(elemvtex%elemvtex)

endsubroutine delete_elemvtex

!------------------------------------------------------------------------------!
! Constructor NEW genelemvtex
!------------------------------------------------------------------------------!
subroutine new_genelemvtex(genelemvtex, nsection)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(out) :: genelemvtex
integer(kpp),         intent(in)  :: nsection

! -- internal variables --

genelemvtex%nsection = nsection
if (nsection > 0) then
  allocate(genelemvtex%elem(nsection))
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

if (genelemvtex%nsection >= 1) then
  do i = 1, genelemvtex%nsection
    call delete_elemvtex(genelemvtex%elem(i))
  enddo
  deallocate(genelemvtex%elem)
endif
genelemvtex%nsection = 0

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

dim = genelemvtex%nsection

allocate(tab_elem(dim+1))

if (dim >= 1) then                             ! if existing ELEMVTEX sections
  tab_elem(1:dim) = genelemvtex%elem(1:dim)
endif

genelemvtex%elem  => tab_elem
genelemvtex%nsection =  dim + 1

endsubroutine addelem_genelemvtex

!------------------------------------------------------------------------------!
! Function : compute number of VTEX PER FACES in ELEMENT DEFINITION
!------------------------------------------------------------------------------!
function number_element(genelemvtex, dim) result(sum)
implicit none
integer :: sum
! -- dummy arguments --
type(st_genelemvtex), intent(in)  :: genelemvtex
integer, optional,    intent(in)  :: dim
! -- internal variables --
integer :: i, idim

sum = 0
do i = 1, genelemvtex%nsection
  if (present(dim)) then
    if (dim_element(genelemvtex%elem(i)) == dim) &
      sum = sum + genelemvtex%elem(i)%nelem
  else
    sum = sum + genelemvtex%elem(i)%nelem
  endif
enddo

endfunction number_element

!------------------------------------------------------------------------------!
! GET index of type in genelemvtex
!------------------------------------------------------------------------------!
function getindex_genelemvtex(genelemvtex, itype, istart) result(isection)
implicit none
integer                           :: isection
! -- dummy arguments --
type(st_genelemvtex), intent(in)  :: genelemvtex
integer,              intent(in)  :: itype
integer, optional,    intent(in)  :: istart

! -- internal variables --
integer :: i

if (present(istart)) then
  isection = istart
else
  isection = 1
endif
do while (isection <= genelemvtex%nsection)
  if (genelemvtex%elem(isection)%elemtype == itype) exit
  isection = isection + 1
enddo
if (isection > genelemvtex%nsection) isection = 0

endfunction getindex_genelemvtex

!------------------------------------------------------------------------------!
! subroutine MERGE same element sections
!------------------------------------------------------------------------------!
subroutine merge_genelemvtex(genelemvtex)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(inout) :: genelemvtex

! -- internal variables --
type(st_elemvtex)    :: elemvtex
integer              :: i, ielem, isame, nelem, itype, dim, nvtex

ielem = 1
do while (ielem <= genelemvtex%nsection)
  itype = genelemvtex%elem(ielem)%elemtype
  if (itype /= elem_null) then
    nelem = 0
    isame = ielem
    ! -- count number of element of same type(ielem) --
    do while (isame /= 0)
      nelem = nelem + genelemvtex%elem(isame)%nelem
      isame = getindex_genelemvtex(genelemvtex, itype, isame+1)
    enddo
    ! -- merge sections --
    if (nelem > genelemvtex%elem(ielem)%nelem) then
      nvtex = genelemvtex%elem(ielem)%nvtex
      call new_elemvtex(elemvtex, nelem, itype, nvtex)
      isame = ielem
      nelem = 0
      do while (isame /= 0)
        dim = genelemvtex%elem(isame)%nelem
        print*,'!!!DEBUG: merge ',ielem, dim, nelem
        elemvtex%ielem   (nelem+1:nelem+dim)          = genelemvtex%elem(isame)%ielem   (1:dim)
        elemvtex%elemvtex(nelem+1:nelem+dim, 1:nvtex) = genelemvtex%elem(isame)%elemvtex(1:dim, 1:nvtex)
        nelem = nelem + dim
        call delete_elemvtex(genelemvtex%elem(isame)) ! element type is set to ielem_null
        isame = getindex_genelemvtex(genelemvtex, itype, isame+1)
      enddo
      genelemvtex%elem(ielem) = elemvtex
    endif
  endif
  ielem = ielem + 1
enddo

endsubroutine merge_genelemvtex

!------------------------------------------------------------------------------!
! subroutine PACK : delete empty, non allocated or ielem_null sections
!------------------------------------------------------------------------------!
subroutine pack_genelemvtex(genelemvtex)
implicit none
! -- dummy arguments --
type(st_genelemvtex), intent(inout) :: genelemvtex

! -- internal variables --
type(st_genelemvtex) :: save
integer              :: nelem, ielem, dim

dim = size(genelemvtex%elem)

call new_genelemvtex(save, dim)

nelem = 0
do ielem = 1, dim
  if ((genelemvtex%elem(ielem)%elemtype /= elem_null).and. &
      (associated(genelemvtex%elem(ielem)%elemvtex))) then
    nelem = nelem + 1
    save%elem(nelem) = genelemvtex%elem(ielem)
  endif
enddo
deallocate(genelemvtex%elem)
call new_genelemvtex(genelemvtex, nelem)
genelemvtex%elem(1:nelem) = save%elem(1:nelem)
deallocate(save%elem)

endsubroutine pack_genelemvtex


!------------------------------------------------------------------------------!
endmodule ELEMVTEX
!------------------------------------------------------------------------------!
! Change history
! Apr 2008 : creation, element/vtex connectivity (derived from USTMESH)
! Oct 2009 : transfered from TYPHON sources
! Dec 2010 : merge same element sections
!------------------------------------------------------------------------------!

