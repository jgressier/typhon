!------------------------------------------------------------------------------!
! Procedure : calc_jacflux                         Authors : J. Gressier
!                                                  Created : Aug  2005
! Fonction                                
!   Computes HLL Jacobian matrices of a numerical flux
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_jacflux(defns, nflux, face, cell, vn, ideb, jac)
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
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell             ! champs des valeurs primitives
real(krp), dimension(1:nflux) &
                      :: vn

! -- Outputs --
type(st_mattab)       :: jac              ! jac associees

! -- Inputs --
integer               :: if
type(v3d)             :: fn
real(krp)             :: g, g1, ig1, en, v2

! -- Body --

g   = defns%properties(1)%gamma
g1  = g - 1._krp
ig1 = 1._krp/g1

! -- Calculs preliminaires --

jac%mat(1:5, 1:5, ideb:ideb-1+nflux) = 0._krp

do if = 1, nflux

  fn = face(if)%normale 
  v2 = sqrabs(cell(if)%velocity)
  en = ig1*cell(if)%pressure/cell(if)%density + .5_krp*v2

  ! -- mass jacobian --

  jac%mat(1, 3:5, ideb-1+if) = tab(fn)

  ! -- energy jacobian --

  jac%mat(2, 1,   ideb-1+if) = vn(if)*(g*en - g1*v2)
  jac%mat(2, 2,   ideb-1+if) = vn(if)*g
  jac%mat(2, 3:5, ideb-1+if) = (g*en - .5_krp*g1*v2)*tab(fn) - g1*vn(if)*tab(cell(if)%velocity)

  ! -- momentum jacobian --

  jac%mat(3:5,   1, ideb-1+if) = .5_krp*g1*v2*tab(fn) - vn(if)*tab(cell(if)%velocity)
  jac%mat(3:5,   2, ideb-1+if) = g1*tab(fn)
  jac%mat(3:5, 3:5, ideb-1+if) = tab((cell(if)%velocity.tens.fn)) - tab(((g1*fn).tens.cell(if)%velocity))
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
