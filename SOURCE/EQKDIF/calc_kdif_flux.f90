!------------------------------------------------------------------------------!
! Procedure : calc_kdif_flux              Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  :
!   Calcul des flux de conduction de la chaleur  Fn = - conduc * (grad T . n)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_kdif_flux(defsolver, nflux, face, cg_l, cell_l, cg_r, cell_r, flux)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MESHBASE
use DEFFIELD
use EQKDIF
use GEO3D

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: defsolver        ! type d'équation à résoudre
integer               :: nflux            ! nombre de flux (face) à calculer
type(st_face),     dimension(1:nflux) & 
                      :: face             ! données géométriques des faces
type(v3d),         dimension(1:nflux) &
                      :: cg_l, cg_r       ! centres des cellules
type(st_kdifetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives

! -- Declaration des sorties --
real(krp), dimension(nflux, defsolver%nequat) :: flux

! -- Declaration des variables internes --
integer   :: if
real(krp), dimension(:), allocatable :: conduct
real(krp) :: dist
type(v3d) :: dcg

! -- Debut de la procedure --

allocate(conduct(nflux))

select case(defsolver%defkdif%materiau%type)
case(mat_LIN)
  !print*,"!! DEBUG-conduction :", defsolver%defkdif%materiau%Kd%valeur
  conduct(:) = defsolver%defkdif%materiau%Kd%valeur
case(mat_KNL, mat_XMAT)
  call erreur("Calcul de matériau","Materiau non linéaire interdit")
endselect

do if = 1, nflux
  dcg         = cg_r(if) - cg_l(if)
  dist        = abs(dcg)
  flux(if,1)  = - conduct(if) * (cell_r(if)%temperature - cell_l(if)%temperature) &
                              * (dcg.scal.face(if)%normale) / (dist**2)
  !print*,"!! DEBUG-flux",if,":",dist,flux(if,1)
enddo

deallocate(conduct)

endsubroutine calc_kdif_flux

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
