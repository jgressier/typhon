!------------------------------------------------------------------------------!
! Procedure : readcgns_ustboco  
!                               
! Fonction                      
!   Lecture de conditions aux limites pour maillage non structure
!
!------------------------------------------------------------------------------!
subroutine readcgns_ustboco(unit, ib, iz, ibc, boco)                 

use DEF_USTBOCO 
use IOCFD
use STRING
use CGNS_STRUCT

implicit none

! -- INPUTS --
integer            :: unit         ! numero d'unite pour la lecture
integer            :: ib, iz, ibc  ! numero de base, zone et condition aux limites

! -- OUTPUTS --
type(st_ustboco) :: boco         ! structure "condition aux limites"

#ifdef CGNS

! -- Internal Variables --                                        
integer             :: ier         ! code erreur
integer             :: bctyp       ! type   de condition aux limites
integer             :: pttyp       ! type   de reference
integer             :: npts        ! nombre de sommets references
integer             :: iloc        ! nombre de sommets references
integer             :: n1, n2, n3, nd ! variables fantomes
character(len=cgnslen) :: cgnsname  ! nom fantome

! --- BODY ---


!-------------------------------------------
! CGNS BOCO information

boco%family    = ""
cgnsname       = ""
boco%ilocation = 0

call cg_boco_info_f(unit, ib, iz, ibc, cgnsname, bctyp, pttyp, npts, &
                    n1, n2, n3, nd, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot get boundary conditions information")

!-------------------------------------------
! Check family name according to boco definition

select case(bctyp)

case(FamilySpecified)
  call cfd_error("(CGNS) CGNS/boco tag not implemented")

case default
  call cg_goto_f(unit, ib, ier, 'Zone_t', iz, 'ZoneBC_t', 1, 'BC_t',ibc, 'end')
  if (ier /= 0) call cfd_error("(CGNS) cannot browse boundary conditions")
  call cg_famname_read_f(boco%family, ier)
  if (ier /= 0) then
     boco%family = cgnsname
  endif
  call cg_gridlocation_read_f(iloc, ier)
  if (ier /= 0) then
    call cfd_warning("(CGNS) Lecture CGNS : type de connectivite non defini (VERTEX par defaut)")
    boco%ilocation = iloc_vtex ! cgns grid location : Vertex
  else
    select case(iloc)
    case(Vertex)
      boco%ilocation = iloc_vtex
    case(CellCenter)
      boco%ilocation = iloc_elemcell
    case(FaceCenter)
      boco%ilocation = iloc_elemface
    case(IFaceCenter, JFaceCenter, KFaceCenter, EdgeCenter)
      call cfd_error("(CGNS) BOCO grid location not implemented")
    case default
      call cfd_error("(CGNS) BOCO grid location unknown")
    endselect
  endif
endselect

call cfd_print("  > tagged element section"//strof(ibc)//" : "//trim(boco%family))
 
!-------------------------------------------
! read BOCO connectivity

boco%ntag = npts
allocate(boco%itag(boco%ntag))

select case(pttyp)

case(ElementList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%itag(1:boco%ntag), npts, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read boundary condition content")
  call cfd_warning("(CGNS) undefined boundary condition tagging method (default is FACE tag)")
  boco%ilocation = iloc_elemface ! check if necessary

case(PointList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%itag(1:boco%ntag), npts, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read boundary condition content")

case(PointRange)
  call cfd_error("(CGNS) unexpected list definition (PointRange)")

case default
  call cfd_error("(CGNS) unknown list definition")
endselect

call cfd_print("    type "//trim(BCTypeName(bctyp))//", "//strofr(npts,6)//" "//&
               trim(str_location(boco%ilocation))//" tags")

#else
  call cfd_error("CGNS has not been activated during compilation")
#endif


!------------------------------
endsubroutine readcgns_ustboco
!------------------------------------------------------------------------------!
! History
!
! Nov  2002: created 
! Dec  2010: direct fill-in of USTMESH/USTBOCO structure
!------------------------------------------------------------------------------!
