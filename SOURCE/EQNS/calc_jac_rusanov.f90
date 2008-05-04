!------------------------------------------------------------------------------!
! Procedure : calc_jac_rusanov                         Authors : J. Gressier
!                                                  Created : Aug  2005
! Fonction                                
!   Computes RUSANOV Jacobian matrices of a numerical flux
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_jac_rusanov(defsolver, defspat, nflux, face,        &
                        cell_l, cell_r, ray, vnl, vnr, ideb, jacL, jacR)
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
type(st_nsetat)       :: cell_l, cell_r   ! champs des valeurs primitives
real(krp), dimension(nflux) &
                      :: ray, vnl, vnr 

! -- Outputs --
type(st_mattab)       :: jacL, jacR  ! jac associees

! -- Internal parameters --
integer(kip) :: i

! -- Body --

call calc_jacflux(defsolver%defns, nflux, face, cell_l, vnl, ideb, jacL)
call calc_jacflux(defsolver%defns, nflux, face, cell_r, vnr, ideb, jacR)

do i = 1, 5
  jacL%mat(i, i, ideb:ideb-1+nflux) = jacL%mat(i, i, ideb:ideb-1+nflux) + ray(1:nflux)
  jacR%mat(i, i, ideb:ideb-1+nflux) = jacR%mat(i, i, ideb:ideb-1+nflux) - ray(1:nflux)
enddo

do i = 1, nflux
  jacL%mat(1:5, 1:5, ideb-1+i) = .5_krp*jacL%mat(1:5, 1:5, ideb-1+i)
  jacR%mat(1:5, 1:5, ideb-1+i) = .5_krp*jacR%mat(1:5, 1:5, ideb-1+i)
enddo



endsubroutine calc_jac_rusanov

!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2008 : creation
!------------------------------------------------------------------------------!
