!------------------------------------------------------------------------------!
! Procedure : readcgns_ustboco            Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cd historique)
!   Lecture de conditions aux limites pour maillage non structure
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgns_ustboco(unit, ib, iz, ibc, boco)                 

use CGNSLIB       ! definition des mots-clefs
use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON
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
if (ier /= 0) call erreur("Lecture CGNS","Probleme a la lecture des conditions aux limites")

! --- nom de famille selon le type de definition ---

select case(bctyp)

case(FamilySpecified)
  call erreur("Developpement","Cas CGNS/boco non implemente")

case default
  call cg_goto_f(unit, ib, ier, 'Zone_t', iz, 'ZoneBC_t', 1, 'BC_t',ibc, 'end')
  if (ier /= 0) call erreur("Lecture CGNS","Probleme lors du parcours ADF")
  call cg_famname_read_f(boco%family, ier)
  if (ier /= 0) then
     boco%family = boco%nom
  endif
  call cg_gridlocation_read_f(boco%gridlocation, ier)
  if (ier /= 0) then
    call print_warning("Lecture CGNS : type de connectivite non defini (VERTEX par defaut)")
    boco%gridlocation = Vertex
  endif
endselect
 
!write(str_w,*) 
call print_info(5, ". tagged element section"//strof(ibc,3)//" : "//trim(boco%family))
call print_info(8, "    type "//trim(BCTypeName(bctyp))//", "//strof(npts,6)//" items")

! --- Lecture des noeuds ou faces references --

call new(boco%list, npts)

select case(pttyp)

case(ElementList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%list%fils(1:npts), n1, ier)
  if (ier /= 0) call erreur("Lecture CGNS", "Probleme a la lecture des conditions aux limites")
  call print_warning("Lecture CGNS : type de connectivite non defini (FACE par defaut)")
  boco%gridlocation = FaceCenter

case(PointList)
  call cg_boco_read_f(unit, ib, iz, ibc, boco%list%fils(1:npts), n1, ier)
  if (ier /= 0) call erreur("Lecture CGNS", "Probleme a la lecture des conditions aux limites")

case(PointRange)
  call erreur("Gestion CGNS","type de reference (PointRange) inattendu en non structure")

case default
  call erreur("Gestion CGNS","type de reference inattendu dans cette version CGNS")

endselect


!------------------------------
endsubroutine readcgns_ustboco
