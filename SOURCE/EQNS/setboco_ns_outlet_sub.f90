!------------------------------------------------------------------------------!
! Procedure : setboco_ns_outlet_sub       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of supersonic inlet boundary conditions
!   
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_outlet_sub(defns, unif, bc_ns, ustboco, umesh, fld)

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
integer         :: icell, ighost    ! index de cellule interieure, et de cellule fictive
type(st_nsetat) :: nspri
real(krp)       :: pi, ti, mach
type(v3d)       :: dir

! -- Debut de la procedure --

if (unif /= uniform) call erreur("Developpement","Condition non uniforme non implementee")

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  icell  = umesh%facecell%fils(if,1)
  ighost = umesh%facecell%fils(if,2)
  call nspri2pi_ti_mach_dir(defns%properties(1), &
                            rho_ps_vel2nspri(fld%etatprim%tabscal(1)%scal(icell), &
                                             fld%etatprim%tabscal(2)%scal(icell), &
                                             fld%etatprim%tabvect(1)%vect(icell)),  &
                            pi, ti, mach, dir) 
  nspri = pi_ti_ps_dir2nspri(defns%properties(1), pi, ti, bc_ns%pstat, dir) 

  fld%etatprim%tabscal(1)%scal(ighost) = nspri%density
  fld%etatprim%tabscal(2)%scal(ighost) = nspri%pressure
  fld%etatprim%tabvect(1)%vect(ighost) = nspri%velocity
enddo


endsubroutine setboco_ns_outlet_sub

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
!------------------------------------------------------------------------------!
