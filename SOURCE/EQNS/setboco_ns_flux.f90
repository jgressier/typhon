!------------------------------------------------------------------------------!
! Procedure : setboco_ns_flux             Auteur : J. Gressier/E. Radenac
!                                         Date   : June 2005
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur, mur à flux imposé
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_flux(defns, defale, defmrf, unif, ustboco, umesh, fld, bcns, curtime)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_BOCO
use MENU_ALE
use MESHMRF
use USTMESH
use DEFFIELD 

implicit none

! -- INPUTS --
type(mnu_ns)       :: defns            ! solver parameters
type(mnu_ale)      :: defale           ! ALE parametres
type(mnu_mrf)      :: defmrf           ! MRF parametres
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! boundary condition location
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(st_boco_ns)   :: bcns             ! parameters and temperature (field or constant)
real(krp)          :: curtime          ! current time

! -- OUTPUTS --
type(st_field)   :: fld            ! fields

! -- Internal variables --
integer    :: ifb, if, ip  ! index de liste, index de face limite et parametres
integer    :: ic, ighost   ! index de cellule interieure, et de cellule fictive
integer    :: nface
real(krp)  :: r_PG, cp     ! perfect gas constant, heat capacity
real(krp)  :: temp, conduct, d, gTdc, gPdc
real(krp), dimension(1) &
           :: TH, mu       ! cell temperature , viscocity
type(v3d)  :: cgface, cg, normale ! face, cell center, face normale
type(v3d)  :: gradT        ! temperature gradient
type(v3d)  :: dc           ! vector cell center - its projection 
                           ! on the face normale
real(krp), allocatable :: lflux(:)
type(v3d)  :: wallvelocity

! -- BODY --

nface = ustboco%nface
allocate(lflux(nface))

r_PG = defns%properties(1)%r_const                                       ! perfect gas constant
cp   = defns%properties(1)%gamma*r_PG/(defns%properties(1)%gamma-1._krp) ! heat capacity

select case(unif)
case(uniform)  
  lflux = bcns%flux  !!! USER flux is entering the domain (so, must be reversed)
                       ! already reversed in def_boco_ns

case(nonuniform)
  lflux(1:nface) = bcns%flux_nunif(1:nface)

case default
  call error_stop("unknown definition of boco flux computation (ns)")
endselect

do ifb = 1, ustboco%nface

  if     = ustboco%iface(ifb)
  ighost = umesh%facecell%fils(if,2)
  ic     = umesh%facecell%fils(if,1)

  cgface = umesh%mesh%iface(if,1,1)%centre
  cg     = umesh%mesh%centre(ic,1,1)
  normale= umesh%mesh%iface(if,1,1)%normale
  d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))

  TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * fld%etatprim%tabscal(1)%scal(ic) )

  call calc_viscosity(defns%properties(1), fld%etatprim%tabscal(1)%scal(ic:ic), TH(1:1), mu(1:1))

  conduct = mu(1) * cp / defns%properties(1)%prandtl

  temp = fld%etatprim%tabscal(2)%scal(ic) / &
       ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) - lflux(ifb)*d/conduct

  ! heat flux
  ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) + lflux(ifb)

  ! pressure
  fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic)

  ! density
  fld%etatprim%tabscal(1)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*temp) 

  ! velocity
  wallvelocity = bcns%wall_velocity
  call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%iface(if,1,1), if, curtime)
  fld%etatprim%tabvect(1)%vect(ighost) = (2._krp*wallvelocity) - fld%etatprim%tabvect(1)%vect(ic)
  
enddo

deallocate(lflux)

endsubroutine setboco_ns_flux

!------------------------------------------------------------------------------!
! Changes history
!
! jun  2005: creation
! sept 2005: changed to ghost cell (velocity is symmetrical)
! June 2009: simplification and bug correction (must not use gradients)
! Apr  2011: wall velocity updated to account for MRF and ALE (A.Gardi)
!------------------------------------------------------------------------------!
