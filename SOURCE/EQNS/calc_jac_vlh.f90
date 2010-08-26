!------------------------------------------------------------------------------!
! Procedure : calc_jac_vlh                      Authors : J. Gressier
!
! Function
!   Computes Jacobian matrices based on van Leer / Hanel flux
!
!------------------------------------------------------------------------------!
subroutine calc_jac_vlh(defsolver, defspat, nflux, face,        &
                        cell_l, cell_r, mnl, mnr, al, ar, ideb, jacL, jacR)
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
type(mnu_solver)      :: defsolver        ! solver parameters
type(mnu_spat)        :: defspat          ! space integration parameters
integer               :: nflux            ! number of fluxes
integer               :: ideb             ! index of first flux (offset)
type(st_face), dimension(1:nflux) &
                      :: face             ! geom. data of faces
type(st_nsetat)             :: cell_l, cell_r   ! primitive variables array
real(krp), dimension(nflux) :: mnl, mnr, al, ar

! -- Inputs/Outputs --

! -- Outputs --
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal parameters --
real(krp), dimension(nflux)   :: V2
real(krp), dimension(nflux)   :: fmn, dmn, fp, dfp ! Mach and pressure functions (and derivatives)
real(krp), dimension(5,nflux) :: dMdQ, dKdQ, QH    ! gradient of quantity, QH (Q with H instead of E)
real(krp)                     :: g, g1, ig1, gg1sd, gsg1
integer(kip) :: i, j, iend

! -- Body --

g     = defsolver%defns%properties(1)%gamma
g1    = g - 1._krp
ig1   = 1._krp/g1
gg1sd = .5_krp*g*g1
gsg1  = g/g1

! -- Initialization --

iend = ideb-1+nflux
!jac%mat(1:5, 1:5, ideb:iend) = 0._krp

!-------------------------------
! LEFT jacobian (dF+/dQ)

where (mnl(1:nflux) >= 1._krp)
  fmn(1:nflux) = mnl(1:nflux)
  dmn(1:nflux) = 1._krp
  fp (1:nflux) = 1._krp
  dfp(1:nflux) = 0._krp
elsewhere (mnl(1:nflux) <= -1._krp)
  fmn(1:nflux) = 0._krp
  dmn(1:nflux) = 0._krp
  fp (1:nflux) = 0._krp
  dfp(1:nflux) = 0._krp
elsewhere
  fmn(1:nflux) = +.25_krp*(mnl(1:nflux)+1._krp)**2
  dmn(1:nflux) = +.5_krp*(mnl(1:nflux)+1._krp)
  fp (1:nflux) = fmn(1:nflux) * (2._krp - mnl(1:nflux))
  dfp(1:nflux) = +.75_krp*(1._krp-mnl(1:nflux)**2)
endwhere

V2(1:nflux) = sqrabs(cell_l%velocity(1:nflux))

! -- dMndQ --

dMdQ(1, 1:nflux) = -.5_krp*mnl(1:nflux)/cell_l%density(1:nflux)*(gg1sd*V2(1:nflux)/al(1:nflux)**2 + 1._krp)
dMdQ(2, 1:nflux) = -.5_krp*g1*mnl(1:nflux)/cell_l%pressure(1:nflux) 

do i = 1, nflux
  dMdQ(3:5, i) = (tab(face(i)%normale)+(gg1sd*mnl(i)/al(i))*tab(cell_l%velocity(i))) / (cell_l%density(i)*al(i))
enddo

! -- dKdQ = dM+/dM * a * [dMn/dQ] + M+ * [da/dQ] --

dKdQ(1, 1:nflux) = dmn(1:nflux)*al(1:nflux)* dMdQ(1, 1:nflux) + fmn(1:nflux)* &
                   .5_krp/cell_l%density(1:nflux)*(gg1sd*V2(1:nflux)/al(1:nflux)-al(1:nflux))
dKdQ(2, 1:nflux) = dmn(1:nflux)*al(1:nflux)* dMdQ(2, 1:nflux) + fmn(1:nflux)* &
                   gg1sd/(cell_l%density(1:nflux)*al(1:nflux))
QH(1, 1:nflux) = cell_l%density(1:nflux)
QH(2, 1:nflux) = gsg1*cell_l%pressure(1:nflux) + .5_krp*cell_l%density(1:nflux)*V2(1:nflux)

do i = 1, nflux
  dKdQ(3:5, i) = dmn(i)*al(i)* dMdQ(3:5, i) + fmn(i)* &
                 ((-gg1sd)/(cell_l%density(i)*al(i)))*tab(cell_l%velocity(i))
  QH(3:5, i) = cell_l%density(i)*tab(cell_l%velocity(i))
enddo

! tensorial product + diagonal term

do i = 1, 5
  do j = 1, 5
    jacL%mat(i, j, ideb:iend) = QH(i, 1:nflux) * dKdQ(j, 1:nflux)                ! tensorial product
  enddo
  ! add diagonal term
  jacL%mat(i, i, ideb:iend) = jacL%mat(i,   i, ideb:iend) + fmn(1:nflux)*al(1:nflux) 
enddo

! -- dKdQ = [dp/dQ] --

dKdQ(1, 1:nflux) = .5_krp*g1*V2(1:nflux)
dKdQ(2, 1:nflux) = g1

do i = 1, nflux
  dKdQ(3:5, i) = -g1*tab(cell_l%velocity(i))
  QH  (3:5, i) = tab(face(i)%normale)
enddo

! -- energy term : M+ * a * dp/dQ -- 

do j = 1, 5
  jacL%mat(2, j, ideb:iend) = jacL%mat(2, j, ideb:iend) + & 
                                      (fmn(1:nflux)*al(1:nflux))*dKdQ(j, 1:nflux) 
enddo

! -- dKdQ = p+ * dp/dQ + dp+/dM * dM/dQ  -- 

do j = 1, 5
  dKdQ(j, 1:nflux) = fp(1:nflux)*dKdQ(j, 1:nflux) + cell_l%pressure(1:nflux)*dfp(1:nflux)*dMdQ(j, 1:nflux)
enddo

! -- momentum term : n.tens.( p+ * dp/dQ + p * dp+/dM * dM/dQ )  -- 

do i = 3, 5 ! momentum
  do j = 1, 5
    jacL%mat(i, j, ideb:iend) = jacL%mat(i, j, ideb:iend) + QH(i, 1:nflux)*dKdQ(j, 1:nflux)
  enddo
enddo

!-------------------------------
! RIGHT jacobian (dF-/dQ)

where (mnr(1:nflux) >= 1._krp)
  fmn(1:nflux) = 0._krp
  dmn(1:nflux) = 0._krp
  fp (1:nflux) = 0._krp
  dfp(1:nflux) = 0._krp
elsewhere (mnr(1:nflux) <= -1._krp)
  fmn(1:nflux) = mnr(1:nflux)
  dmn(1:nflux) = 1._krp
  fp (1:nflux) = 1._krp
  dfp(1:nflux) = 0._krp
elsewhere
  fmn(1:nflux) = -.25_krp*(mnr(1:nflux)-1._krp)**2
  dmn(1:nflux) = -.5_krp*(mnr(1:nflux)-1._krp)
  fp (1:nflux) = -fmn(1:nflux) * (2._krp + mnr(1:nflux))
  dfp(1:nflux) = -.75_krp*(1._krp-mnr(1:nflux)**2)
endwhere

V2(1:nflux) = sqrabs(cell_r%velocity(1:nflux))

! -- dMndQ --

dMdQ(1, 1:nflux) = -.5_krp*mnr(1:nflux)/cell_r%density(1:nflux)*(gg1sd*V2(1:nflux)/ar(1:nflux)**2 + 1._krp)
dMdQ(2, 1:nflux) = -.5_krp*g1*mnr(1:nflux)/cell_r%pressure(1:nflux) 

do i = 1, nflux
  dMdQ(3:5, i) = (tab(face(i)%normale)+(gg1sd*mnr(i)/ar(i))*tab(cell_r%velocity(i)))/(cell_r%density(i)*ar(i))
enddo

! -- dKdQ = dM-/dM * a * [dMn/dQ] + M- * [da/dQ] --

dKdQ(1, 1:nflux) = dmn(1:nflux)*ar(1:nflux)* dMdQ(1, 1:nflux) + fmn(1:nflux)* &
                   .5_krp/cell_r%density(1:nflux)*(gg1sd*V2(1:nflux)/ar(1:nflux)-ar(1:nflux))
dKdQ(2, 1:nflux) = dmn(1:nflux)*ar(1:nflux)* dMdQ(2, 1:nflux) + fmn(1:nflux)* &
                   gg1sd/(cell_r%density(1:nflux)*ar(1:nflux))
QH(1, 1:nflux) = cell_r%density(1:nflux)
QH(2, 1:nflux) = gsg1*cell_r%pressure(1:nflux) + .5_krp*cell_r%density(1:nflux)*V2(1:nflux)

do i = 1, nflux
  dKdQ(3:5, i) = dmn(i)*ar(i)* dMdQ(3:5, i) + fmn(i)* &
                 ((-gg1sd)/(cell_r%density(i)*ar(i)))*tab(cell_r%velocity(i))
  QH(3:5, i) = cell_r%density(i)*tab(cell_r%velocity(i))
enddo

! tensorial product + diagonal term

do i = 1, 5
  do j = 1, 5
    jacR%mat(i, j, ideb:iend) = QH(i, 1:nflux) * dKdQ(j, 1:nflux)                ! tensorial product
  enddo
  ! add diagonal term
  jacR%mat(i, i, ideb:iend) = jacR%mat(i,   i, ideb:iend) + fmn(1:nflux)*ar(1:nflux) 
enddo

! -- dKdQ = [dp/dQ] --

dKdQ(1, 1:nflux) = .5_krp*g1*V2(1:nflux)
dKdQ(2, 1:nflux) = g1

do i = 1, nflux
  dKdQ(3:5, i) = -g1*tab(cell_r%velocity(i))
  QH  (3:5, i) = tab(face(i)%normale)
enddo

! -- energy term : M- * a * dp/dQ -- 

do j = 1, 5
  jacR%mat(2, j, ideb:iend) = jacR%mat(2, j, ideb:iend) + & 
                                      (fmn(1:nflux)*ar(1:nflux))*dKdQ(j, 1:nflux)                ! tensorial product
enddo

! -- dKdQ = p- * dp/dQ + dp-/dM * dM/dQ  -- 

do j = 1, 5
  dKdQ(j, 1:nflux) = fp(1:nflux)*dKdQ(j, 1:nflux) + cell_r%pressure(1:nflux)*dfp(1:nflux)*dMdQ(j, 1:nflux)
enddo

! -- momentum term : n.tens.( p- * dp/dQ + dp-/dM * dM/dQ )  -- 

do i = 3, 5 ! momentum
  do j = 1, 5
    jacR%mat(i, j, ideb:iend) = jacR%mat(i, j, ideb:iend) + QH(i, 1:nflux)*dKdQ(j, 1:nflux)
  enddo
enddo

endsubroutine calc_jac_vlh

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2010 : creation
!------------------------------------------------------------------------------!
