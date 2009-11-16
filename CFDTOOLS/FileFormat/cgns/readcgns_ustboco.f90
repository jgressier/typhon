!------------------------------------------------------------------------------!
! Procedure : readcgns_ustboco            Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cd historique)
!   Lecture de conditions aux limites pour maillage non structure
!
!------------------------------------------------------------------------------!

subroutine readcgns_ustboco(unit, ib, iz, ibc, boco)                 

use CGNS_STRUCT   ! Definition des structures CGNS
use IOCFD
use STRING

implicit none

! -- Entrees --
integer            :: unit         ! numero d'unite pour la lecture
integer            :: ib, iz, ibc  ! numero de base, zone et condition aux limites

! -- Sorties --
type(st_cgns_boco) :: boco         ! structure "condition aux limites"

! -- Variables internes --                                        
integer             :: ier         ! code erreur
integer             :: bctyp       ! type   de condition aux limites
integer             :: pttyp       ! type   de reference
integer             :: npts        ! nombre de sommets references
integer             :: n1, n2, n3, nd ! variables fantomes

! -- Debut de procedure


! --- Lecture des informations ---

boco%nom = ""
call cg_boco_info_f(unit, ib, iz, ibc, boco%nom, bctyp, pttyp, npts, &
                    n1, n2, n3, nd, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot get boundary conditions information")

! --- nom de famille selon le type de definition ---

select case(bctyp)

case(FamilySpecified)
  call cfd_error("(CGNS) CGNS/boco tag not implemented")

case default
  call cg_goto_f(unit, ib, ier, 'Zone_t', iz, 'ZoneBC_t', 1, 'BC_t',ibc, 'end')
  if (ier /= 0) call cfd_error("(CGNS) cannot browse boundary conditions")
  call cg_famname_read_f(boco%family, ier)
  if (ier /= 0) then
     boco%family = boco%nom
  endif
  call cg_gridlocation_read_f(boco%gridlocation, ier)
  if (ier /= 0) then
    call cfd_warning("(CGNS) Lecture CGNS : type de connectivite non defini (VERTEX par defaut)")
    boco%gridlocation = Vertex
  endif
endselect
 
call cfd_print(". tagged element section"//strof(ibc,3)//" : "//trim(boco%family))
call cfd_print("    type "//trim(BCTypeName(bctyp))//", "//strof(npts,6)//" items")

! --- Lecture des noeuds ou faces references --

call new(boco%list, npts)

select case(pttyp)

case(ElementList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%list%fils(1:npts), n1, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read boundary condition content")
  call cfd_warning("(CGNS) undefined boundary condition tagging method (default is FACE tag)")
  boco%gridlocation = FaceCenter

case(PointList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%list%fils(1:npts), n1, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read boundary condition content")

case(PointRange)
  call cfd_error("(CGNS) unexpected list definition (PointRange)")

case default
  call cfd_error("(CGNS) unknown list definition")

endselect


!------------------------------
endsubroutine readcgns_ustboco
