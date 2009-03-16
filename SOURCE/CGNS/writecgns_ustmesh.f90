!------------------------------------------------------------------------------!
! Procedure : writecgns_ustmesh                      Authors : J. Gressier
!                                                    Created : February 2006
! Fonction 
!   Write an GRID USTMESH to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine writecgns_ustmesh(cgnsunit, ibase, izone, umesh) 

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use USTMESH

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
integer               :: ibase, izone       ! base and zone index
type(st_ustmesh)      :: umesh

! -- OUPUTS --

! -- Internal variables --
integer               :: i, dim, ufc, ir, istart, iend
integer               :: info, cgnstype, ielem, nvtex, nelem, icoord
real(8), allocatable  :: v(:)

! -- BODY --

! -- write vertices --

allocate(v(umesh%nvtex))
do i = 1, umesh%nvtex
  v(i) = umesh%mesh%vertex(i,1,1)%x
enddo
call cg_coord_write_f(cgnsunit, ibase, izone, RealDouble, 'CoordinateX', v, icoord, info)
do i = 1, umesh%nvtex
  v(i) = umesh%mesh%vertex(i,1,1)%y
enddo
call cg_coord_write_f(cgnsunit, ibase, izone, RealDouble, 'CoordinateY', v, icoord, info)
do i = 1, umesh%nvtex
  v(i) = umesh%mesh%vertex(i,1,1)%z
enddo
call cg_coord_write_f(cgnsunit, ibase, izone, RealDouble, 'CoordinateZ', v, icoord, info)
deallocate(v)

iend = 0

do ielem = 1, umesh%cellvtex%ntype

  select case(umesh%cellvtex%elem(ielem)%elemtype)
  case(elem_bar2)
    cgnstype = BAR_2
  case(elem_tri3)
    cgnstype = TRI_3
  case(elem_quad4)
    cgnstype = QUAD_4
  case(elem_ngon)
    cgnstype = NGON_n
  case(elem_tetra4)
    cgnstype = TETRA_4
  case(elem_pyra5)
    cgnstype = PYRA_5
  case(elem_penta6)
    cgnstype = PENTA_6 ! Wedge
  case(elem_hexa8)
    cgnstype = HEXA_8
  case default
    call erreur("CGNS writer", "do not known how to write this element type")
  endselect

  nelem  = umesh%cellvtex%elem(ielem)%nelem
  istart = iend   +1
  iend   = istart -1 + nelem
  nvtex  = umesh%cellvtex%elem(ielem)%nvtex

  call cg_section_write_f(cgnsunit, ibase, izone, 'Elem', cgnstype, istart, iend, 0, &
      transpose(umesh%cellvtex%elem(ielem)%elemvtex(1:nelem,1:nvtex)), ielem, info)

enddo

endsubroutine writecgns_ustmesh
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created
!------------------------------------------------------------------------------!
