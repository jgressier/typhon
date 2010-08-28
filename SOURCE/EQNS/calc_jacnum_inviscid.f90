!------------------------------------------------------------------------------!
! Procedure : calc_flux_inviscid                Authors : J. Gressier
!
! Function
!   Computation of numerical jacobians from inviscid flux formulation
!
!------------------------------------------------------------------------------!
subroutine calc_jacnum_inviscid(defsolver, defspat, nflux, ideb, face, &
                                QL, QR, flux, calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! solver parameters
type(mnu_spat)        :: defspat          ! space integration parameters
integer, intent(in)   :: nflux            ! number of fluxes
integer               :: ideb             ! index of first flux
type(st_face)         :: face(nflux)      ! geom. data of faces
type(st_genericfield) :: flux
type(st_nsetat)       :: QL, QR
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
type(st_nsetat)       :: Qmod
type(st_genericfield) :: fluxmod
integer               :: i, ifin, isca, ivec, jjac
real(krp)             :: epsil, drho, drhov, drhoe, g, g1
logical, parameter    :: false = .false.

! -- BODY --

call new_nsetat(Qmod, nflux)

ifin    = ideb+nflux-1
epsil = sqrt(epsilon(epsil))

g  = defsolver%defns%properties(1)%gamma
g1 = g - 1._krp

call new_genericfield(fluxmod, nflux, flux%nscal, flux%nvect, 0)

!------------------------------------------------------------------------------!
! QL Jacobian
!------------------------------------------------------------------------------!
isca = 1  ! DENSITY
jjac = 1

drho = epsil*defsolver%refsca(isca)

Qmod%density (1:nflux) = QL%density (1:nflux) + drho
Qmod%pressure(1:nflux) = QL%pressure(1:nflux) + (.5_krp*g1)*sqrabs(QL%velocity(1:nflux))*drho
Qmod%velocity(1:nflux) = (1._krp - drho/QL%density (1:nflux))*QL%velocity(1:nflux) 

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, Qmod, QR, fluxmod, false, jacL, jacR)

jacL%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drho
jacL%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drho
do i = 1, nflux
  jacL%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drho
enddo

!------------------------------------------------------------------------------!
isca = 2  ! ENERGY
jjac = 2

drhoe = epsil*defsolver%refsca(isca)

Qmod%density (1:nflux) = QL%density (1:nflux) 
Qmod%pressure(1:nflux) = QL%pressure(1:nflux) + g1*drhoe
Qmod%velocity(1:nflux) = QL%velocity(1:nflux) 

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, Qmod, QR, fluxmod, false, jacL, jacR)

jacL%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhoe
jacL%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhoe
do i = 1, nflux
  jacL%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhoe
enddo

!------------------------------------------------------------------------------!
ivec = 1  ! MOMENTUM

drhov = epsil*defsolver%refvec(ivec)

Qmod%density (1:nflux) = QL%density(1:nflux) 

jjac = 3  ! MOMENTUM-X

do i = 1, nflux
  Qmod%pressure(i)   = QL%pressure(i) - g1*QL%velocity(i)%x*drhov
  Qmod%velocity(i)%x = QL%velocity(i)%x + drhov/QL%density(i)
  Qmod%velocity(i)%y = QL%velocity(i)%y
  Qmod%velocity(i)%z = QL%velocity(i)%z
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, Qmod, QR, fluxmod, false, jacL, jacR)

jacL%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacL%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacL%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

jjac = 4  ! MOMENTUM-Y

do i = 1, nflux
  Qmod%pressure(i)   = QL%pressure(i) - g1*QL%velocity(i)%y*drhov
  Qmod%velocity(i)%x = QL%velocity(i)%x 
  Qmod%velocity(i)%y = QL%velocity(i)%y + drhov/QL%density(i)
  Qmod%velocity(i)%z = QL%velocity(i)%z
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, Qmod, QR, fluxmod, false, jacL, jacR)

jacL%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacL%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacL%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

jjac = 5  ! MOMENTUM-Z

do i = 1, nflux
  Qmod%pressure(i)   = QL%pressure(i) - g1*QL%velocity(i)%z*drhov
  Qmod%velocity(i)%x = QL%velocity(i)%x
  Qmod%velocity(i)%y = QL%velocity(i)%y
  Qmod%velocity(i)%z = QL%velocity(i)%z + drhov/QL%density(i)
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, Qmod, QR, fluxmod, false, jacL, jacR)

jacL%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacL%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacL%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

!------------------------------------------------------------------------------!
! QR Jacobian
!------------------------------------------------------------------------------!
isca = 1  ! DENSITY
jjac = 1

drho = epsil*defsolver%refsca(isca)

Qmod%density (1:nflux) = QR%density (1:nflux) + drho
Qmod%pressure(1:nflux) = QR%pressure(1:nflux) + (.5_krp*g1)*sqrabs(QR%velocity(1:nflux))*drho
Qmod%velocity(1:nflux) = (1._krp - drho/QR%density (1:nflux))*QR%velocity(1:nflux) 

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, QL, Qmod, fluxmod, false, jacL, jacR)

jacR%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drho
jacR%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drho
do i = 1, nflux
  jacR%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drho
enddo

!------------------------------------------------------------------------------!
isca = 2  ! ENERGY
jjac = 2

drhoe = epsil*defsolver%refsca(isca)

Qmod%density (1:nflux) = QR%density (1:nflux) 
Qmod%pressure(1:nflux) = QR%pressure(1:nflux) + g1*drhoe
Qmod%velocity(1:nflux) = QR%velocity(1:nflux) 

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, QL, Qmod, fluxmod, false, jacL, jacR)

jacR%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhoe
jacR%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhoe
do i = 1, nflux
  jacR%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhoe
enddo

!------------------------------------------------------------------------------!
ivec = 1  ! MOMENTUM

drhov = epsil*defsolver%refvec(ivec)

Qmod%density (1:nflux) = QR%density(1:nflux) 

jjac = 3  ! MOMENTUM-X

do i = 1, nflux
  Qmod%pressure(i)   = QR%pressure(i) - g1*QR%velocity(i)%x*drhov
  Qmod%velocity(i)%x = QR%velocity(i)%x + drhov/QR%density(i)
  Qmod%velocity(i)%y = QR%velocity(i)%y
  Qmod%velocity(i)%z = QR%velocity(i)%z
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, QL, Qmod, fluxmod, false, jacL, jacR)

jacR%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacR%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacR%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

jjac = 4  ! MOMENTUM-Y

do i = 1, nflux
  Qmod%pressure(i)   = QR%pressure(i) - g1*QR%velocity(i)%y*drhov
  Qmod%velocity(i)%x = QR%velocity(i)%x 
  Qmod%velocity(i)%y = QR%velocity(i)%y + drhov/QR%density(i)
  Qmod%velocity(i)%z = QR%velocity(i)%z
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, QL, Qmod, fluxmod, false, jacL, jacR)

jacR%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacR%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacR%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

jjac = 5  ! MOMENTUM-Z

do i = 1, nflux
  Qmod%pressure(i)   = QR%pressure(i) - g1*QR%velocity(i)%z*drhov
  Qmod%velocity(i)%x = QR%velocity(i)%x
  Qmod%velocity(i)%y = QR%velocity(i)%y
  Qmod%velocity(i)%z = QR%velocity(i)%z + drhov/QR%density(i)
enddo

! called with ideb=1 because of fluxmod sizes
call calc_flux_inviscid(defsolver, defspat, nflux, 1, face, QL, Qmod, fluxmod, false, jacL, jacR)

jacR%mat(1, jjac, ideb:ifin) = (fluxmod%tabscal(1)%scal(1:nflux) -flux%tabscal(1)%scal(ideb:ifin)) &
                           / drhov
jacR%mat(2, jjac, ideb:ifin) = (fluxmod%tabscal(2)%scal(1:nflux) -flux%tabscal(2)%scal(ideb:ifin)) &
                           / drhov
do i = 1, nflux
  jacR%mat(3:5, jjac, ideb-1+i) = tab(fluxmod%tabvect(1)%vect(i) -flux%tabvect(1)%vect(ideb-1+i)) &
                           / drhov
enddo

endsubroutine calc_jacnum_inviscid

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2010 : creation
!------------------------------------------------------------------------------!
