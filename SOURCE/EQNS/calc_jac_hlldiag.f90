!------------------------------------------------------------------------------!
! Procedure : calc_jac_hlldiag                         Authors : J. Gressier
!                                                      Created : Aug  2005
! Fonction                                
!   Computes HLL Jacobian matrices : diagonal simplified expression
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_jac_hlldiag(defsolver, defspat, nflux, face,        &
                        cell_l, cell_r, sl, sr, vnl, vnr, ideb, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
real(krp), dimension(nflux) &
                      :: sl, sr, vnl, vnr 

! -- Outputs --
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal parameters --
integer(kip) :: i

! -- Body --

jacL%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp
jacR%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp
do i = 1, 5
  jacL%mat(i, i, ideb:ideb-1+nflux) = sr(1:nflux)
  jacR%mat(i, i, ideb:ideb-1+nflux) = sl(1:nflux)
enddo

endsubroutine calc_jac_hlldiag

!------------------------------------------------------------------------------!
! Changes history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!
