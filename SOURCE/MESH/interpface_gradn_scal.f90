!------------------------------------------------------------------------------!
! Procedure : interp_facegradient_scal    Author : J. Gressier
!                                         Date   : March 2005
! Fonction                                Modif  : (cf history)
!   Compute an interpolated gradient at a face center from cell gradients
!   and cell values
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine interp_facegradn_scal(nf, meth, ndHL, ndHR, vLR, face, &
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
type(st_face), dimension(nf) :: face       ! geomtrical face array
!
real(krp), dimension(nf) ::  qL,  qR   ! left and right quantities (at cell centers)
type(v3d), dimension(nf) :: dqL, dQR   ! left and right gradients  (at cell centers)

! -- OUTPUTS --
real(krp), dimension(nf) :: dqH        ! interpolated face gradients 


! -- Internal variables --
real(krp) :: dLR2(nf)
real(krp) :: pscal, Favg, Fcomp
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
! COMPACT : F1 = (T(R) - T(L))            ! L & R cell centers
! AVERAGE : F2 = (a.gT(L) + b.gT(R)).n    ! H face center
! FULL    : F3 = 
! a = HR/RL et b = HL/RL

select case(meth)

case(dis_celldif2) ! compact formulation, non consistent if vLR and (n) are not aligned

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    dqH(if)  =  (qR(if) - qL(if)) / dLR2(if) * (vLR(if).scal.face(if)%normale)
  enddo

case(dis_cellavg2) ! consistent formulation, only weighted average of gradients

  do if = 1, nf
    dqH(if)  = ((ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.face(if)%normale)
  enddo

case(dis_cellfull) ! full consistent formulation, averaged between compact and averaged formulations

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    pscal = vLR(if).scal.face(if)%normale / dLR2(if)
    Fcomp = pscal * (qR(if) - qL(if))
    vi    = face(if)%normale - (theta*pscal)*vLR(if)
    Favg  = (ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.vi
    dqH(if) = theta*Fcomp + Favg
  enddo

case default
  call error_stop("internal error: unknown gradient interpolation method (interpface_gradn_scal)")
endselect

!-----------------------------
endsubroutine interp_facegradn_scal

!------------------------------------------------------------------------------!
! Changes history
!
! mar  2005 : creation
!------------------------------------------------------------------------------!
