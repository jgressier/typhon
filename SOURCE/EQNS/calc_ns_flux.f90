!------------------------------------------------------------------------------!
! Procedure : calc_ns_flux                Auteur : J. Gressier
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des flux convection-diffusion
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_ns_flux(defsolver, nflux, face, cg_l, cell_l, cg_r, cell_r, flux)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: defsolver        ! type d'équation à résoudre
integer               :: nflux            ! nombre de flux (face) à calculer
type(st_face),     dimension(1:nflux) & 
                      :: face             ! données géométriques des faces
type(v3d),         dimension(1:nflux) &
                      :: cg_l, cg_r       ! centres des cellules
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives

! -- Declaration des sorties --
real(krp), dimension(nflux, defsolver%nequat) :: flux

! -- Declaration des variables internes --
integer   :: if
real(krp) :: dist
type(v3d) :: dcg
real(krp) :: tempf   ! température estimée entre deux cellules
real(krp) :: dl, dr  ! distance centre de cellule - face

! -- Debut de la procedure --



endsubroutine calc_ns_flux

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003  : création de la procédure
!------------------------------------------------------------------------------!
