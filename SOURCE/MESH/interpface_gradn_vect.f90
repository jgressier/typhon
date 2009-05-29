!------------------------------------------------------------------------------!
! Procedure : interp_facegradient_vect    Author : J. Gressier
!                                         Date   : March 2005
! Fonction                                Modif  : (cf history)
!   Compute an interpolated gradient at a face center from cell gradients
!   and cell values
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine interp_facegradn_vect(nf, meth, ndHL, ndHR, vLR, face, &
                                 qL, qR, dqL, dqR, dqH)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use MENU_NUM
use USTMESH
use GENFIELD

implicit none

! -- Declaration des entrees --
integer                      :: nf         ! number of parsed faces
integer(kpp)                 :: meth       ! method for gradient interpolation
real(krp),     dimension(nf) :: ndHL       ! face center to left  center distance / (HL + HR)
real(krp),     dimension(nf) :: ndHR       ! face center to right center distance / (HL + HR)
type(v3d),     dimension(nf) :: vLR        ! left center to right center vector 
type(st_face), dimension(nf) :: face       ! geomtrical face array
!
type(v3d), dimension(nf) ::  qL,  qR   ! left and right quantities (at cell centers)
type(t3d), dimension(nf) :: dqL, dQR   ! left and right gradients  (at cell centers)

! -- Declaration des sorties --
type(v3d), dimension(nf) :: dqH        ! interpolated face gradients 


! -- Declaration des variables internes --
real(krp) :: dLR2(taille_buffer)
real(krp) :: pscal
type(v3d) :: Favg, Fcomp
type(v3d) :: vi
integer   :: if, k, icl, icr

real(krp) :: theta = 1._krp

! -- Debut de la procedure --

if (nf > taille_buffer) then
  call erreur("Development","interpolation should be used after splitting into packets")
endif

!--------------------------------------------------------------
! Choice of interpolation formulation
!--------------------------------------------------------------
! COMPACT : F1 = (T(R) - T(L))            ! L & R cell centers
! AVERAGE : F2 = (a.gT(L) + b.gT(R)).n    ! H face center
! FULL    : F3 = 
! a = HR/RL et b = HL/RL

select case(meth)

case(dis_dif2) ! compact formulation, non consistent if vLR and (n) are not aligned

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    dqH(if)  = ( (vLR(if).scal.face(if)%normale) / dLR2(if)) * (qR(if) - qL(if)) 
  enddo

case(dis_avg2) ! consistent formulation, only weighted average of gradients

  do if = 1, nf
    dqH(if)  = ((ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.face(if)%normale)
  enddo

case(dis_full) ! full consistent formulation, averaged between compact and averaged formulations

  dLR2(1:nf) = sqrabs(vLR(1:nf))

  do if = 1, nf
    pscal = vLR(if).scal.face(if)%normale / dLR2(if)
    Fcomp = pscal * (qR(if) - qL(if))
    vi    = face(if)%normale - (theta*pscal)*vLR(if)
    Favg  = (ndHL(if)*dqR(if) + ndHR(if)*dqL(if)).scal.vi
    dqH(if) = theta*Fcomp + Favg
  enddo

endselect

!-----------------------------
endsubroutine interp_facegradn_vect

!------------------------------------------------------------------------------!
! Changes history
!
! mar  2005 : creation
!------------------------------------------------------------------------------!
