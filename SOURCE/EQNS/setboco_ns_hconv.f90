!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_hconv          Auteur : J. Gressier/E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of non uniform "convection like" boundary conditions for 
!   the fluid
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_hconv(defns, defale, defmrf, unif, ustboco, umesh, fld, bcns, curtime)

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
type(st_ustboco)   :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh)   :: umesh            ! unstructured mesh
type(st_boco_ns)   :: bcns           ! parameters and fluxes (field or constant)
real(krp)          :: curtime          ! current time

! -- Declaration des sorties --
type(st_field)   :: fld            ! champ des etats

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer          :: ic, ighost    ! index de cellule interieure, et de cellule fictive
type(v3d)        :: cgface, cg, normale ! centre de face, de cellule, normale face
real(krp)        :: d             ! distance cellule - face limite
real(krp)        :: conduct       ! conductivite
real(krp)        :: r_PG, cp     ! perfect gas constant, heat capacity
real(krp)        :: temp, gTdc, gPdc
type(v3d)        :: gradT        ! temperature gradient
real(krp), dimension(1) &
                 :: TH, mu       ! cell temperature , viscocity
type(v3d)        :: dc           ! vector cell center - its projection 
                                 ! on the face normale
type(v3d)  :: wallvelocity

! -- Debut de la procedure --

r_PG = defns%properties(1)%r_const        ! perfect gas constant
cp = defns%properties(1)%gamma * r_PG / &
     (defns%properties(1)%gamma - 1)      ! heat capacity

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = umesh%facecell%fils(if,1)
  ighost = umesh%facecell%fils(if,2)

  ! Computation of "distance" cell center - face center
  cgface = umesh%mesh%iface(if,1,1)%centre
  cg     = umesh%mesh%centre(ic,1,1)
  normale= umesh%mesh%iface(if,1,1)%normale
  d = abs((cgface - cg).scal.normale)
!  d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale

  ! Temperature, viscosity, conductivity

  TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * fld%etatprim%tabscal(1)%scal(ic) )

  call calc_viscosity(defns%properties(1), fld%etatprim%tabscal(1)%scal(ic:ic), TH(1:1), mu(1:1))

  conduct = mu(1) * cp / defns%properties(1)%prandtl

  ! Approximate computation of temperature in factice cells
  ! (for computation of gradients)
  gradT = 1/( fld%etatprim%tabscal(1)%scal(ic) * r_PG) * &
          fld%gradient%tabvect(2)%vect(ic) - &
          fld%etatprim%tabscal(2)%scal(ic) / &
          ( fld%etatprim%tabscal(1)%scal(ic)**2 * r_PG) * &
          fld%gradient%tabvect(1)%vect(ic)
  gTdc = gradT .scal. dc
  temp = ( (conduct/d) * (fld%etatprim%tabscal(2)%scal(ic) / &
           ( fld%etatprim%tabscal(1)%scal(ic) * r_PG ) + gTdc ) + &
           bcns%tconv_nunif(ifb)*bcns%h_nunif(ifb) ) / &
         (conduct/d+bcns%h_nunif(ifb))
 
  ! Heat flux
  ustboco%bocofield%tabscal(1)%scal(ifb) = ustboco%bocofield%tabscal(1)%scal(ifb) &
                                          + bcns%h_nunif(ifb)*(temp - bcns%tconv_nunif(ifb))

  ! pressure
  gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
  fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                         gPdc
  ! density
  fld%etatprim%tabscal(1)%scal(ighost) = &
              fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*temp)

  ! velocity
  !fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)  
  wallvelocity = bcns%wall_velocity
  call calc_wallvelocity(defale, defmrf, wallvelocity, umesh%mesh%iface(if,1,1), if, curtime)
  fld%etatprim%tabvect(1)%vect(ighost) = (2._krp*wallvelocity) - fld%etatprim%tabvect(1)%vect(ic)

enddo

endsubroutine setboco_ns_hconv

!------------------------------------------------------------------------------!
! Historique des modifications
!
! aug  2005 : creation of routine
! Apr  2011: wall velocity updated to account for MRF and ALE (A.Gardi)
!------------------------------------------------------------------------------!

