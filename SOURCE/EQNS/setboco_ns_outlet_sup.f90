!------------------------------------------------------------------------------!
! Procedure : setboco_ns_outlet_sup       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of supersonic inlet boundary conditions
!   
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_outlet_sup(defns, unif, bc_ns, ustboco, umesh, fld)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrées --
type(mnu_ns)     :: defns            ! solver parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: bc_ns            ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structuré

! -- Declaration des sorties --
type(st_field)   :: fld              ! fld des états

! -- Declaration des variables internes --
integer         :: ifb, if, ip      ! index de liste, index de face limite et paramètres
integer         :: icell, ighost    ! index de cellule intérieure, et de cellule fictive

! -- Debut de la procedure --

if (unif /= uniform) call erreur("Développement","Condition non uniforme non implémentée")

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  icell  = umesh%facecell%fils(if,1)
  ighost = umesh%facecell%fils(if,2)
  fld%etatprim%tabscal(1)%scal(ighost) = fld%etatprim%tabscal(1)%scal(icell)
  fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(icell)
  fld%etatprim%tabvect(1)%vect(ighost) = fld%etatprim%tabvect(1)%vect(icell)
enddo


endsubroutine setboco_ns_outlet_sup

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
!------------------------------------------------------------------------------!
