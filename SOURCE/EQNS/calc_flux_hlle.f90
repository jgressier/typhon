!------------------------------------------------------------------------------!
! Procedure : calc_flux_hlle              Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Computation of HLLE flux for Euler equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_hlle(defsolver, defspat, nflux, face,        &
                          cg_l, cell_l, cg_r, cell_r, flux, ideb, &
                          calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: defsolver        ! paramètres de définition du solveur
type(mnu_spat)        :: defspat          ! paramètres d'intégration spatiale
integer               :: nflux            ! nombre de flux (face) à calculer
integer               :: ideb             ! indice du premier flux à remplir
type(st_face),     dimension(1:nflux) & 
                      :: face             ! données géométriques des faces
type(v3d),         dimension(1:nflux) &
                      :: cg_l, cg_r       ! centres des cellules
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Declaration des sorties --
type(st_field)               :: flux
real(krp), dimension(nflux)  :: jacL, jacR  ! jac associées

! -- Declaration des variables internes --
integer                   :: if

! -- Debut de la procedure --


! -- Calculs préliminaires --

do if = 1, nflux

enddo


!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then
  call erreur("Développement","Calcul de jacobiennes du flux HLLE non implémenté")
endif
!  do if = 1, nflux
!    jacR(if) =  - kH(if) * (vLR(if).scal.face(if)%normale) &
!                  / (defsolver%defkdif%materiau%Cp * dLR(if)**2)
!    jacL(if) = -jacR(if)
!  enddo
!endif


!deallocate()


endsubroutine calc_flux_hlle

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : creation, HLLE flux
!------------------------------------------------------------------------------!
