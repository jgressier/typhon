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
integer         :: ifb, if, ip, nf  ! index de liste, index de face limite et parametres
integer         :: icell, ighost    ! index de cellule interieure, et de cellule fictive
type(st_nsetat) :: nspri
real(krp), allocatable :: ps(:), pi(:), ti(:), mach(:)
type(v3d), allocatable :: dir(:)

! -- Body --

if (unif /= uniform) call erreur("Developpement","Condition non uniforme non implementee")

nf = ustboco%nface
call new(nspri, nf)

do ifb = 1, nf
  if     = ustboco%iface(ifb)
  icell  = umesh%facecell%fils(if,1)
  nspri%density(ifb)  = fld%etatprim%tabscal(1)%scal(icell)
  nspri%pressure(ifb) = fld%etatprim%tabscal(2)%scal(icell)
  nspri%velocity(ifb) = fld%etatprim%tabvect(1)%vect(icell)
enddo

allocate(ps(nf))
allocate(pi(nf))
allocate(ti(nf))
allocate(mach(nf))
allocate(dir(nf))

call nspri2pi_ti_mach_dir(defns%properties(1), nf, nspri, pi, ti, mach, dir) 
ps(1:nf) = bc_ns%pstat

call pi_ti_ps_dir2nspri(defns%properties(1), nf, pi(1:nf), ti(1:nf), ps(1:nf), dir(1:nf), nspri) 

do ifb = 1, nf
  if     = ustboco%iface(ifb)
  ighost = umesh%facecell%fils(if,2)
  fld%etatprim%tabscal(1)%scal(ighost) = nspri%density(ifb)
  fld%etatprim%tabscal(2)%scal(ighost) = nspri%pressure(ifb)
  fld%etatprim%tabvect(1)%vect(ighost) = nspri%velocity(ifb)
enddo

call delete(nspri)
deallocate(ps, pi, ti, mach, dir)

endsubroutine setboco_ns_outlet_sub

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation
! mar  2006 : array computation (optimization)
!------------------------------------------------------------------------------!
