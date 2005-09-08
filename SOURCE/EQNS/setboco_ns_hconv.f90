!------------------------------------------------------------------------------!
! Procedure : setboco_kdif_hconv          Auteur : J. Gressier/E. Radenac
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf Historique)
!   Computation of non uniform "convection like" boundary conditions for 
!   the fluid
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine setboco_ns_hconv(defns, unif, ustboco, ustdom, fld, flux, defsolver, bcns)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrees --
type(mnu_ns)       :: defns            ! solver parameters
integer            :: unif             ! uniform or not
type(st_ustboco)   :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh)   :: ustdom           ! maillage non structure
type(mnu_solver)   :: defsolver        ! type d'equation a resoudre
type(st_boco_ns)   :: bcns           ! parameters and fluxes (field or constant)

! -- Declaration des sorties --
type(st_field)   :: fld            ! champ des etats
real(krp), dimension(ustboco%nface) &
                 :: flux             ! flux

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

! -- Debut de la procedure --

r_PG = defns%properties(1)%r_const        ! perfect gas constant
cp = defns%properties(1)%gamma * r_PG / &
     (defns%properties(1)%gamma - 1)      ! heat capacity

do ifb = 1, ustboco%nface
  if     = ustboco%iface(ifb)
  ic     = ustdom%facecell%fils(if,1)
  ighost = ustdom%facecell%fils(if,2)

  ! Computation of "distance" cell center - face center
  cgface = ustdom%mesh%iface(if,1,1)%centre
  cg     = ustdom%mesh%centre(ic,1,1)
  normale= ustdom%mesh%iface(if,1,1)%normale
  d = abs((cgface - cg).scal.normale)
!  d    = (cgface - cg) .scal. (cgface - cg) / (abs((cgface - cg).scal.normale))
  dc = (cgface - cg) - ( (cgface - cg).scal.normale ) * normale

  ! Temperature
  ! Computation of conductivity
  select case(defns%typ_visc)
  case(visc_suth)
    TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
            fld%etatprim%tabscal(1)%scal(ic) )
    call calc_visc_suther(defns, 1, TH, mu, 1)
  case(visc_cst)
    mu(1) = defns%properties(1)%visc_dyn
  case(visc_lin)
    TH(1) = fld%etatprim%tabscal(2)%scal(ic) / (r_PG * &
            fld%etatprim%tabscal(1)%scal(ic) )
    mu(1) = defns%properties(1)%visc_dyn*TH(1)
  case default
    call erreur("viscosity computation","unknown kind of computation")
  endselect
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
  flux(ifb) = bcns%h_nunif(ifb)*(temp - bcns%tconv_nunif(ifb))

  ! pressure
  gPdc = fld%gradient%tabvect(2)%vect(ic) .scal. dc
  fld%etatprim%tabscal(2)%scal(ighost) = fld%etatprim%tabscal(2)%scal(ic) + &
                                         gPdc
  ! density
  fld%etatprim%tabscal(1)%scal(ighost) = &
              fld%etatprim%tabscal(2)%scal(ighost)/(r_PG*temp)

  ! velocity
  fld%etatprim%tabvect(1)%vect(ighost) = v3d(0._krp,0._krp,0._krp)  

enddo

endsubroutine setboco_ns_hconv

!------------------------------------------------------------------------------!
! Historique des modifications
!
! aug  2005 : creation of routine
!------------------------------------------------------------------------------!

