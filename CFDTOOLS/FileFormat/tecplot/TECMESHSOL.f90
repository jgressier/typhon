!------------------------------------------------------------------------------!
! MODULE : TECMESHSOL
!
!------------------------------------------------------------------------------!
module TECMESHSOL

use IOCFD
use STRING
use PACKET
use TECFMT
use QUANTITY
use USTMESH
use GENFIELD

implicit none

! -- Global Variables -------------------------------------------


! -- Parameters --

!------------------------------------------------------------------------------!
! 
!------------------------------------------------------------------------------!
!type st_deftec
!endtype st_deftec

contains 
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! write TEC header definition
!------------------------------------------------------------------------------!
subroutine tec_ini(deftec, filetype, title, gfield, info)
implicit none
! -- INPUTS --
type(st_deftec)       :: deftec
character(len=*)      :: title
integer(teckip)       :: filetype
!type(st_ustmesh)      :: umesh
type(st_genericfield) :: gfield
! -- OUTPUTS --
integer              :: info
! -- private data --
integer              :: isca, ivec
! -- BODY --

info  = 0

write(deftec%iunit) filetype
call tecwrite_str(deftec%iunit, title)

deftec%nvar = 3 + gfield%nscal + 3*gfield%nvect

write(deftec%iunit) deftec%nvar

call tecwrite_str(deftec%iunit, "X")
call tecwrite_str(deftec%iunit, "Y")
call tecwrite_str(deftec%iunit, "Z")

do isca = 1, gfield%nscal
  call tecwrite_str(deftec%iunit, trim(quantity_name(gfield%tabscal(isca)%quantity_id)))
enddo
do ivec = 1, gfield%nvect
  call tecwrite_str(deftec%iunit, trim(quantity_name(gfield%tabvect(ivec)%quantity_id))//"_X")
  call tecwrite_str(deftec%iunit, trim(quantity_name(gfield%tabvect(ivec)%quantity_id))//"_Y")
  call tecwrite_str(deftec%iunit, trim(quantity_name(gfield%tabvect(ivec)%quantity_id))//"_Z")
enddo

allocate(deftec%location(deftec%nvar))
deftec%location(1:3)               = tecloc_node
deftec%location(4:deftec%nvar)     = tecloc_cell

endsubroutine tec_ini

!------------------------------------------------------------------------------!
! write TEC mesh and solution ZONE HEADER
!------------------------------------------------------------------------------!
subroutine tecwrite_allzoneheader(deftec, umesh, gfield, nsol)
implicit none
! -- INPUTS --
type(st_deftec)       :: deftec
type(st_ustmesh)      :: umesh
type(st_genericfield) :: gfield
integer               :: nsol
! -- OUTPUTS --
! -- private data --
integer(teckip)              :: elemtype, nfacecon, strandid, parent, sharezonecon
integer(teckip), allocatable :: location(:)
real(teckrp)                 :: time
integer                      :: info, i

! -- BODY --

info  = 0

call tec_ini(deftec, tecfile_full, 'TYPHON', gfield, info)

if (info /=0) call cfd_error("unable to init tecplot file")

!------------------------------------------------------
! DEFINE ZONE

deftec%ncell = umesh%ncell_int
deftec%nnode = umesh%nvtex

select case(umesh%elemdim)
case(2)
  deftec%zonetype = tec_fequad
case(3)
  deftec%zonetype = tec_febrick
case default
  call cfd_error("internal error: unknown mesh geometric dimension")
endselect

time      = 0.
strandid  = 0
parent    = 0     ! no parent

call tecwrite_zoneheader(deftec, 'mesh', parent, strandid, time, info)

if (info /=0) call cfd_error("unable to write tecplot zone")

do i = 1, nsol
  time      = nsol
  strandid  = 0
  parent    = 0     ! no parent
  call tecwrite_zoneheader(deftec, 'solution '//strof(i), parent, strandid, time, info)
  if (info /=0) call cfd_error("unable to write tecplot zone")
enddo

write(deftec%iunit) eohmarker

endsubroutine tecwrite_allzoneheader

!------------------------------------------------------------------------------!
! write TEC mesh data
!------------------------------------------------------------------------------!
subroutine tecwrite_ustmesh(deftec, umesh)
implicit none
! -- INPUTS --
type(st_deftec)      :: deftec
type(st_ustmesh)     :: umesh
! -- OUTPUTS --
! -- private data --
integer(teckip)              :: elemtype, nfacecon, sharezonecon
integer(teckip), allocatable :: location(:), passivevar(:), sharezonevar(:), cellvtex(:,:)
integer                      :: i, maxnvtex, ielem, nvtex
integer                      :: info
real(teckrp),    allocatable :: value(:,:)

! -- BODY --

!------------------------------------------------------
! DEFINE DATA

allocate(passivevar(deftec%nvar))
allocate(sharezonevar(deftec%nvar))

passivevar(1:3)             = 0_teckip    ! coordinate are active
passivevar(4:deftec%nvar)   = 1_teckip    ! other var  are passive
sharezonevar(1:deftec%nvar) = 0_teckip    ! no shared variables
sharezonecon                = 0_teckip    ! no shared zone

call tecwrite_dataheader(deftec, passivevar, sharezonevar, sharezonecon, info)

if (info /=0) call cfd_error("unable to write tecplot data header")

!------------------------------------------------------
! vertices coordinates 

allocate(value(deftec%nnode, 3))
do i = 1, deftec%nnode
  value(i, 1) = umesh%mesh%vertex(i,1,1)%x   ! indirection & real*8
  value(i, 2) = umesh%mesh%vertex(i,1,1)%y   ! indirection & real*8
  value(i, 3) = umesh%mesh%vertex(i,1,1)%z   ! indirection & real*8
enddo

call tecwrite_data(deftec, value, info)

if (info /=0) call cfd_error("unable to write tecplot coordinates")

deallocate(value)

!------------------------------------------------------
! CELLVTEX connectivity

select case(deftec%zonetype)
case(tec_fetri)
  call cfd_error("internal error: tec_fetri not yet implemented")
case(tec_fequad)
  maxnvtex = 4
case(tec_fetetra)
  call cfd_error("internal error: tec_fetri not yet implemented")
case(tec_febrick)
  maxnvtex = 8
case default
  call cfd_error("internal error: unknown tecplot element type")
endselect

allocate(cellvtex(maxnvtex, deftec%ncell))

do ielem = 1, umesh%cellvtex%nsection

  if (dim_element(umesh%cellvtex%elem(ielem)) == umesh%elemdim) then  ! filter volumic elements

    nvtex = umesh%cellvtex%elem(ielem)%nvtex

    select case(umesh%cellvtex%elem(ielem)%elemtype)
    case(elem_tri3)
      do i = 1, umesh%cellvtex%elem(ielem)%nelem
        cellvtex(1:nvtex,  umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)
        cellvtex(maxnvtex, umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,nvtex)
      enddo
    case(elem_quad4)
      do i = 1, umesh%cellvtex%elem(ielem)%nelem
        cellvtex(1:maxnvtex, umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)
      enddo
    case(elem_ngon)
      call cfd_error("internal error: ngon element not yet implemented")
    case(elem_tetra4)
      call cfd_error("internal error: tetra element not yet implemented")
    case(elem_pyra5)
      do i = 1, umesh%cellvtex%elem(ielem)%nelem
        cellvtex(1:nvtex,       umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)
        cellvtex(nvtex+1:nvtex, umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,nvtex)
      enddo
    case(elem_penta6)
      call cfd_error("internal error: penta element not yet implemented")
    case(elem_hexa8)
      do i = 1, umesh%cellvtex%elem(ielem)%nelem
        cellvtex(1:maxnvtex, umesh%cellvtex%elem(ielem)%ielem(i)) = umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)
      enddo
    case default
      call cfd_error("internal error: unknown typhon element type")
    endselect

  endif

enddo

cellvtex = cellvtex-1_teckip

write(deftec%iunit) cellvtex

if (info /=0) call cfd_error("unable to write tecplot connectivity")

deallocate(cellvtex)

endsubroutine tecwrite_ustmesh

!------------------------------------------------------------------------------!
! write TEC solution data
!------------------------------------------------------------------------------!
subroutine tecwrite_sol(deftec, umesh, gfield)
implicit none
! -- INPUTS --
type(st_deftec)       :: deftec
type(st_ustmesh)      :: umesh
type(st_genericfield) :: gfield
! -- OUTPUTS --
! -- private data --
integer(teckip)              :: elemtype, nfacecon, sharezonecon
integer(teckip), allocatable :: location(:), passivevar(:), sharezonevar(:)
integer                      :: i, iv, isca, ivec, info
real(teckrp),    allocatable :: value(:,:)

! -- BODY --

info  = 0

allocate(passivevar(deftec%nvar))
allocate(sharezonevar(deftec%nvar))

passivevar(1:deftec%nvar)   = 0_teckip    ! no passive variables
sharezonevar(1:3)           = 1_teckip    ! coordinates shared with zone 1
sharezonevar(4:deftec%nvar) = 0_teckip    ! other variables not shared
sharezonecon                = 1_teckip    ! connectivity shared with zone 1

call tecwrite_dataheader(deftec, passivevar, sharezonevar, sharezonecon, info)

if (info /=0) call cfd_error("unable to write tecplot data header")

!------------------------------------------------------

allocate(value(deftec%ncell, deftec%nvar-3))

do isca = 1, gfield%nscal
  do i = 1, deftec%ncell
    value(i, isca) = gfield%tabscal(isca)%scal(i)   ! indirection & real*8
  enddo
enddo

do ivec = 1, gfield%nvect
  iv = gfield%nscal + (ivec-1)*3
  do i = 1, deftec%ncell
    value(i, iv+1) = gfield%tabvect(ivec)%vect(i)%x   ! indirection & real*8
    value(i, iv+2) = gfield%tabvect(ivec)%vect(i)%y   ! indirection & real*8
    value(i, iv+3) = gfield%tabvect(ivec)%vect(i)%z   ! indirection & real*8
  enddo
enddo

call tecwrite_data(deftec, value, info)

if (info /=0) call cfd_error("unable to write tecplot solution")

deallocate(value)

endsubroutine tecwrite_sol

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
!!$integer function typhon2tec_elemtype(itype)
!!$implicit none
!!$! -- dummy arguments --
!!$integer(kpp),      intent(in)  :: itype
!!$
!!$select case(itype)
!!$case(elem_bar2)
!!$  typhon2tec_elemtype = tec_bar2
!!$case(elem_tri3)
!!$  typhon2tec_elemtype = tec_tri3
!!$case(elem_quad4)
!!$  typhon2tec_elemtype = tec_quad4
!!$case(elem_ngon)
!!$  typhon2tec_elemtype = tec_ngon
!!$case(elem_tetra4)
!!$  typhon2tec_elemtype = tec_tetra4
!!$case(elem_pyra5)
!!$  typhon2tec_elemtype = tec_pyra5
!!$case(elem_penta6)
!!$  typhon2tec_elemtype = tec_penta6
!!$case(elem_hexa8)
!!$  typhon2tec_elemtype = tec_hexa8
!!$case default
!!$  typhon2tec_elemtype = -1
!!$endselect
!!$
!!$endfunction typhon2tec_elemtype


endmodule TECMESHSOL
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created
!------------------------------------------------------------------------------!
