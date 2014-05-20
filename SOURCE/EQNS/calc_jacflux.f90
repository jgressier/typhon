!------------------------------------------------------------------------------!
! Procedure : calc_jacflux                         Authors : J. Gressier
!                                                  Created : Aug  2005
!> @brief Computes HLL Jacobian matrices of a numerical flux
!------------------------------------------------------------------------------!
subroutine calc_jacflux(defns, nflux, fn, cell, vn, ideb, jac)
use TYPHMAKE
use OUTPUT
use VARCOM
use DEFFIELD
use MENU_NS
use GEO3D
use TENSOR3
use MESHBASE
use MATRIX_ARRAY

implicit none

! -- Declaration des entrees --
type(mnu_ns)          :: defns            ! parametres de definition du solveur
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(v3d)             :: fn(nflux)        ! face normals
type(st_nsetat)       :: cell             ! champs des valeurs primitives
real(krp), dimension(1:nflux) &
                      :: vn

! -- Outputs --
type(st_mattab)       :: jac              ! jac associees

! -- Inputs --
integer               :: if
real(krp)             :: g, g1, ig1, en, v2

! -- Body --

g   = defns%properties(1)%gamma
g1  = g - 1._krp
ig1 = 1._krp/g1

! -- Calculs preliminaires --

jac%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp

do if = 1, nflux

  v2 = sqrabs(cell%velocity(if))
  en = ig1*cell%pressure(if)/cell%density(if) + .5_krp*v2

  ! -- mass jacobian --

  jac%mat(1, 1:2, ideb-1+if) = 0._krp
  jac%mat(1, 3:5, ideb-1+if) = tab(fn(if))

  ! -- energy jacobian --

  jac%mat(2, 1,   ideb-1+if) = vn(if)*(g1*v2 - g*en)
  jac%mat(2, 2,   ideb-1+if) = vn(if)*g
  jac%mat(2, 3:5, ideb-1+if) = (g*en - .5_krp*g1*v2)*tab(fn(if)) - g1*vn(if)*tab(cell%velocity(if))

  ! -- momentum jacobian --

  jac%mat(3:5,   1, ideb-1+if) = .5_krp*g1*v2*tab(fn(if)) - vn(if)*tab(cell%velocity(if))
  jac%mat(3:5,   2, ideb-1+if) = g1*tab(fn(if))
  jac%mat(3:5, 3:5, ideb-1+if) = tab((cell%velocity(if).tens.fn(if))) - tab(((g1*fn(if)).tens.cell%velocity(if)))
  jac%mat(3,3, ideb-1+if) = jac%mat(3,3, ideb-1+if) + vn(if)
  jac%mat(4,4, ideb-1+if) = jac%mat(4,4, ideb-1+if) + vn(if)
  jac%mat(5,5, ideb-1+if) = jac%mat(5,5, ideb-1+if) + vn(if)

enddo


endsubroutine calc_jacflux

!------------------------------------------------------------------------------!
! Changes history
!
! July 2005 : creation, Euler fluxes
!------------------------------------------------------------------------------!
