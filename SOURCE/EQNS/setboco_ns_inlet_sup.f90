!------------------------------------------------------------------------------!
! Procedure : setboco_ns_inlet_sup        Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of supersonic inlet boundary conditions
!   
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_inlet_sup(defns, unif, bc_ns, ustboco, umesh, fld)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrees --
type(mnu_ns)     :: defns            ! solver parameters
integer          :: unif             ! uniform or not
type(st_boco_ns) :: bc_ns            ! parameters (field or constant)
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: umesh            ! maillage non structure

! -- Declaration des sorties --
type(st_field)   :: fld              ! fld des etats

! -- Declaration des variables internes --
integer         :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer         :: ighost           ! index de cellule interieure, et de cellule fictive
type(st_nsetat) :: nspri

! -- Debut de la procedure --

if (unif /= uniform) call erreur("Developpement","Condition non uniforme non implementee")

call new(nspri, 1)

call pi_ti_mach_dir2nspri(defns%properties(1), 1, bc_ns%ptot, bc_ns%ttot, &
                                                  bc_ns%mach, bc_ns%direction, nspri) 

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ighost = umesh%facecell%fils(if,2)
  fld%etatprim%tabscal(1)%scal(ighost) = nspri%density(1)
  fld%etatprim%tabscal(2)%scal(ighost) = nspri%pressure(1)
  fld%etatprim%tabvect(1)%vect(ighost) = nspri%velocity(1)
enddo

call delete(nspri)

endsubroutine setboco_ns_inlet_sup

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
!------------------------------------------------------------------------------!
