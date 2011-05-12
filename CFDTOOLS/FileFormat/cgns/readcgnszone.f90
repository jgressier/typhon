!------------------------------------------------------------------------------!
! Procedure : readcgnszone
!                         
! Function                
!   Fill GRID/USTMESH connectivity by CGNS (zone) connectivity
!
!------------------------------------------------------------------------------!
subroutine readcgnszone(unit, ib, iz, umesh) 

use IOCFD        ! Sorties standard TYPHON
use USTMESH
use STRING
use CGNS_STRUCT

implicit none 

! -- INPUTS --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib, iz     ! numero de base et de zone

! -- OUTPUTS --
type(st_ustmesh) :: umesh

! -- Internal variables --
integer       :: size(3,3)          ! tableau d'informations de la zone
integer       :: ier                ! error code
integer       :: idim, igeo, itype  ! cgns mesh dimension (2/3), ?, cgns mesh type
integer       :: i, ibc             ! index
integer       :: nboco
integer       :: nmax_elem          ! nombre total d'elements
character(len=cgnslen) :: cgnsname  ! nom fantome
character(len=100)     :: str_w     ! nom fantome

! -- BODY --
   
call cg_base_read_f(unit, ib, cgnsname, idim, igeo, ier)

!! BUG : test desactive car ier /= 0 meme si tout est correct
!if (ier /= 0) call erreur("Lecture CGNS","Probleme a la lecture de la base")

!-----------------------------------------------------------------
! read and check CGNS ZONE type 
          
call cg_zone_type_f(unit, ib, iz, itype, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read CGNS zone type")

select case(itype)
  case(Structured, Unstructured)
    call cfd_print("> Zone "//trim(strof(iz))//": type "//ZoneTypeName(itype))
  case default
    call cfd_error("(CGNS) unknown CGNS zone type")
endselect

!-----------------------------------------------------------------
! read CGNS ZONE information

call cg_zone_read_f(unit, ib, iz, cgnsname, size, ier)
if (ier /= 0)   call cfd_error("(CGNS) cannot read CGNS zone information")

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
! Initialization of USTMESH

select case(idim)    ! transfer mesh dimension 
case(2)
  call cfd_print("    mesh is 2D (planar)")
case(3)
  call cfd_print("    mesh is 3D")
endselect

call init_ustmesh(umesh, 1)   ! default values initialization

!-----------------------------------------------------------------
! Read CGNS vertices

allocate(umesh%mesh%vertex(umesh%mesh%idim, umesh%mesh%jdim, umesh%mesh%kdim)) 
call readcgnsvtex(unit, ib, iz, umesh%mesh)
umesh%nvtex = umesh%mesh%nvtex
   
!-----------------------------------------------------------------
! Read CGNS connectivity

select case(itype)
case(Structured)
  call cfd_error("(CGNS) structured multiblock mesh not implemented")
case(Unstructured)
  call readcgns_ustconnect(unit, ib, iz, umesh)
case default
    call cfd_error("(CGNS) unknown CGNS zone "//trim(cgnsname)//" type")
endselect

!-----------------------------------------------------------------
! Read CGNS Boundary Conditions or TAGS

call cg_nbocos_f(unit, ib, iz, nboco, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read number of CGNS boundary conditions")

call cfd_print(". reading "//trim(strof(nboco))//" CGNS boundary conditions")

call createboco(umesh, nboco)

do ibc = 1, umesh%nboco
  select case(itype)
  case(Structured)
    call cfd_error("(CGNS) structured multiblock mesh not implemented")
  case(Unstructured)
    call readcgns_ustboco(unit, ib, iz, ibc, umesh%boco(ibc))
  case default
    call cfd_error("(CGNS) unknown CGNS zone type")
  endselect
enddo

! --- fermeture du fichier ---

call cg_close_f(unit, ier)

!-------------------------
endsubroutine readcgnszone
!------------------------------------------------------------------------------!
! Change history
!
! nov  2002: creation (read CGNS file)
! Dec  2010: directly fill grid/ustmesh connectivity instead of cgns structure
!------------------------------------------------------------------------------!
