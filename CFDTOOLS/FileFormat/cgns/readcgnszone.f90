!------------------------------------------------------------------------------!
! Procedure : readcgnszone
!
! Function
!   Fill GRID/USTMESH connectivity by CGNS (zone) connectivity
!------------------------------------------------------------------------------!
subroutine readcgnszone(iunit, ib, iz, umesh)

use IOCFD        ! Sorties standard TYPHON
use STRING
use CGNS_STRUCT
use USTMESH

implicit none

! -- INPUTS --
integer          , intent(in)  :: iunit      ! unit number for read
integer          , intent(in)  :: ib, iz     ! base and zone numbers

! -- OUTPUTS --
type(st_ustmesh) , intent(out) :: umesh

! -- Internal variables --
integer       :: size(3,3)          ! zone information table
integer       :: ierr               ! error code
integer       :: sdim, igeo, itype  ! cgns mesh dimension (2/3), ?, cgns mesh type
integer       :: i, ibc             ! index
integer       :: nboco
integer       :: nmax_elem          ! total number of elements
character(len=cgnslen) :: cgnsname  ! cgns base name

! -- BODY --

!-----------------------------------------------------------------
! Initialization of USTMESH

call init_ustmesh(umesh, 1)   ! default values initialization

call cg_base_read_f(iunit, ib, cgnsname, sdim, igeo, ierr)

!> @bug commented test because ierr always O even if wrong CGNS reading
!if (ierr /= 0) call erreur("Lecture CGNS","Probleme a la lecture de la base")

!-----------------------------------------------------------------
! read and check CGNS ZONE type

call cg_zone_type_f(iunit, ib, iz, itype, ierr)
if (ierr /= 0) call cfd_error("(CGNS) cannot read CGNS zone type")

select case(itype)
  case(Structured, Unstructured)
    call cfd_print("> Zone "//trim(strof(iz))//": type "//ZoneTypeName(itype))
  case default
    call cfd_error("(CGNS) unknown CGNS zone type")
endselect

!-----------------------------------------------------------------
! read CGNS ZONE information

call cg_zone_read_f(iunit, ib, iz, cgnsname, size, ierr)
if (ierr /= 0)   call cfd_error("(CGNS) cannot read CGNS zone information")

select case(itype)
  case(Structured)     ! cas STRUCTURE
    ! size(1:3, 1:3) contient             (cf CGNS Mid-level Library)
    !     (1:3, 1) le nombre de sommets ni,nj,nk
    !     (1:3, 2) le nombre de cellules
    !     (1:3, 3) ?
    umesh%mesh%idim = size(1,1)
    umesh%mesh%jdim = size(2,1)
    umesh%mesh%kdim = size(3,1)
    call cfd_error("Only able to read CGNS unstructured grids")
  case(Unstructured)   ! cas NON STRUCTURE
    ! size(1:3, 1) contient             (cf CGNS Mid-level Library)
    !     (1, 1) le nombre de sommets
    !     (2, 1) le nombre de cellules
    !     (3, 1) le nombre de sommets aux limites
    ! ce sont en fait les trois premiers entiers du tableau sous forme lineaire
    umesh%mesh%nvtex = size(1,1)
    umesh%ncell      = size(2,1)
    umesh%mesh%idim  = umesh%mesh%nvtex
    umesh%mesh%jdim  = 1
    umesh%mesh%kdim  = 1
  case default
    call cfd_error("(CGNS) unknown CGNS zone "//trim(cgnsname)//" type")
endselect

!-----------------------------------------------------------------
! mesh geometric property

select case(sdim)    ! transfer mesh dimension
case(2)
  call cfd_print("    mesh is 2D (planar)")
case(3)
  call cfd_print("    mesh is 3D")
endselect

!-----------------------------------------------------------------
! Read CGNS vertices

allocate(umesh%mesh%vertex(umesh%mesh%idim, umesh%mesh%jdim, umesh%mesh%kdim))
call readcgnsvtex(iunit, ib, iz, umesh%mesh)
umesh%nvtex = umesh%mesh%nvtex

!-----------------------------------------------------------------
! Read CGNS connectivity

select case(itype)
case(Structured)
  call cfd_error("(CGNS) structured multiblock mesh not implemented")
case(Unstructured)
  call readcgns_ustconnect(iunit, ib, iz, umesh)
case default
    call cfd_error("(CGNS) unknown CGNS zone "//trim(cgnsname)//" type")
endselect

!-----------------------------------------------------------------
! Read CGNS Boundary Conditions or TAGS

call cg_nbocos_f(iunit, ib, iz, nboco, ierr)
if (ierr /= 0) call cfd_error("(CGNS) cannot read number of CGNS boundary conditions")

call cfd_print(". reading "//trim(strof(nboco))//" CGNS boundary conditions")

call createboco(umesh, nboco)

do ibc = 1, umesh%nboco
  select case(itype)
  case(Structured)
    call cfd_error("(CGNS) structured multiblock mesh not implemented")
  case(Unstructured)
    call readcgns_ustboco(iunit, ib, iz, ibc, umesh%boco(ibc))
  case default
    call cfd_error("(CGNS) unknown CGNS zone type")
  endselect
enddo

call check_ustmesh_elements(umesh)

endsubroutine readcgnszone
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation (read CGNS file)
! Dec  2010: directly fill grid/ustmesh connectivity instead of cgns structure
!------------------------------------------------------------------------------!
