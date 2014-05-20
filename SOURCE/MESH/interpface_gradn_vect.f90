!------------------------------------------------------------------------------!
! Procedure : interp_facegradient_vect    Author : J. Gressier
!                                         Date   : March 2005
!> @brief Compute an interpolated gradient at a face center from cell gradients and cell values
!------------------------------------------------------------------------------!
subroutine interp_facegradn_vect(nf, meth, ndHL, ndHR, vLR, fn, &
                                 qL, qR, dqL, dqR, dqH)

use OUTPUT
use PACKET
use MENU_NUM
use USTMESH
use GENFIELD

implicit none

! -- INPUTS --
integer                      :: nf         ! number of parsed faces
integer(kpp)                 :: meth       ! method for gradient interpolation
real(krp),     dimension(nf) :: ndHL       ! face center to left  center distance / (HL + HR)
real(krp),     dimension(nf) :: ndHR       ! face center to right center distance / (HL + HR)
type(v3d),     dimension(nf) :: vLR        ! left center to right center vector 
type(v3d)                    :: fn(nf)  ! normal of faces
!
type(v3d), dimension(nf) ::  qL,  qR   ! left and right quantities (at cell centers)
type(t3d), dimension(nf) :: dqL, dQR   ! left and right gradients  (at cell centers)

! -- OUTPUTS --
type(v3d), dimension(nf) :: dqH        ! interpolated face gradients 


! -- Internal variables --
real(krp) :: dLR2(taille_buffer)
real(krp) :: pscal
type(v3d) :: Favg, Fcomp
type(v3d) :: vi
integer   :: if, k, icl, icr

real(krp) :: theta = 1._krp

! -- BODY --

if (nf > taille_buffer) then
  call error_stop("Development: interpolation should be used after splitting into packets")
endif

!--------------------------------------------------------------
! Choice of interpolation formulation
!--------------------------------------------------------------
! if Favg = (a.gT(L) + b.gT(R))
! COMPACT : F1 = (T(R) - T(L))*eLR.n      ! L & R cell centers ! not consistent
! AVERAGE : F2 = Favg.n                   ! H face center
! FULL    : F3 = (Favg-(Favg.eLR)*eLR).n + F1
! a = HR/RL et b = HL/RL

select case(meth)

case(dis_celldif2) ! compact formulation, non consistent if vLR and (n) are not aligned

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    dqH(if)  = ( (vLR(if).scal.fn(if)) / dLR2(if)) * (qR(if) - qL(if)) 
  enddo

case(dis_cellavg2) ! consistent formulation, only weighted average of gradients

  do if = 1, nf
    dqH(if)  = ((ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.fn(if))
  enddo

case(dis_cellfull) ! full consistent formulation, averaged between compact and averaged formulations

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    pscal = vLR(if).scal.fn(if) / dLR2(if)
    Fcomp = pscal * (qR(if) - qL(if))
    vi    = fn(if) - (theta*pscal)*vLR(if)
    Favg  = (ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.vi
    dqH(if) = theta*Fcomp + Favg
  enddo

case default
  call error_stop("internal error: unknown gradient interpolation method (interpface_gradn_vect)")
endselect

!-----------------------------
endsubroutine interp_facegradn_vect

!------------------------------------------------------------------------------!
! Changes history
!
! mar  2005 : creation
!------------------------------------------------------------------------------!
