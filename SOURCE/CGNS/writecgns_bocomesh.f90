!------------------------------------------------------------------------------!
! Procedure : writecgns_bocomesh                     Authors : J. Gressier
!
! Fonction 
!   Write a GRID elements and boco TAG to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine writecgns_bocomesh(cgnsunit, ibase, izone, umesh) 

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
integer               :: nelemtot, nvtexmax, nv, nf
integer               :: i, j, ib, iff, isec, ielem, info, istart, iend
integer               :: cgnstype, ibc, nface
integer, allocatable  :: iface(:), nelem(:), newindex(:), elem(:,:)
real(8), allocatable  :: v(:)

! -- BODY --

nelemtot = umesh%nface_lim

! -- get face type and reorder --

nvtexmax = umesh%facevtex%nbfils
allocate(nelem(nvtexmax))          ! number of boco face 
nelem(:) = 0

allocate(newindex(1:nelemtot))

do i = 1, nelemtot
  nv = count(umesh%facevtex%fils(umesh%nface_int+i, 1:nvtexmax) /= 0)
  nelem(nv)   = nelem(nv)+1
enddo

! -- collect and write element section --

iend = umesh%ncell_int   ! last ELEMENT index in CGNS file (internal elements)

ielem = umesh%cellvtex%ntype  ! number of cell sections already written
!call cg_nsections_f(cgnsunit, ibase, izone, ielem)

do isec = 1, nvtexmax

  if (nelem(isec) >= 1) then

    select case(isec)
    case(2)
       cgnstype = BAR_2
    case(3)
       cgnstype = TRI_3
    case(4)
       cgnstype = QUAD_4
    case default
       call erreur("Fatal error writing CGNS boco", "unknown element")
    endselect

    allocate(elem(1:isec, 1:nelem(isec)))

    iff = 0
    do i = 1, nelemtot
      if (count(umesh%facevtex%fils(umesh%nface_int+i, 1:nvtexmax) /= 0) == isec) then
        iff = iff +1
        elem(1:isec, iff) = umesh%facevtex%fils(umesh%nface_int+i, 1:isec)
        newindex(i)       = iend + iff
      endif
    enddo

    istart = iend   +1
    iend   = istart -1 + nelem(isec)
    ielem  = ielem  +1                ! section index

    call cg_section_write_f(cgnsunit, ibase, izone, 'ElemBC_'//trim(ElementTypename(cgnstype)), cgnstype, &
         istart, iend, 0, elem(1:isec, 1:nelem(isec)), ielem, info)

    if (info /= 0) &
      call erreur("Fatal error writing CGNS boco elements", "cgnslib IO error")

    deallocate(elem)
  endif
enddo

! -- write BOCO tags --

do ib = 1, umesh%nboco

   nface = umesh%boco(ib)%nface
   allocate(iface(1:nface))
   iface(1:nface) = newindex(umesh%boco(ib)%iface(1:nface)-umesh%nface_int)

   call cg_boco_write_f(cgnsunit, ibase, izone, umesh%boco(ib)%family, UserDefined, ElementList,&
     umesh%boco(ib)%nface, iface, ibc, info)  ! ibc is an ouput index

   if (info /= 0) &
        call erreur("Fatal error writing CGNS boco tags", "cgnslib IO error")

   deallocate(iface)

enddo

endsubroutine writecgns_bocomesh
!------------------------------------------------------------------------------!
! Changes history
!
! May  2009 : created
!------------------------------------------------------------------------------!
