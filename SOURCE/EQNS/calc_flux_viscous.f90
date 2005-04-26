!------------------------------------------------------------------------------!
! Procedure : calc_flux_viscous           Auteur : J. Gressier
!                                         Date   : February 2004
! Fonction                                Modif  : (cf history)
!   Computation of VISCOUS flux for NS equations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_flux_viscous(defsolver, defspat, nflux, ideb, face, &
                             cell_l, cell_r, gradL, gradR, flux,    &
                             calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D

implicit none

! -- Declaration des entrees --
type(mnu_solver)      :: defsolver        ! parametres de definition du solveur
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nflux            ! nombre de flux (face) a calculer
integer               :: ideb             ! indice du premier flux a remplir
type(st_face), dimension(1:nflux) & 
                      :: face             ! donnees geometriques des faces
type(st_nsetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
type(st_genericfield) :: fgrad
logical               :: calc_jac         ! choix de calcul de la jacobienne


! -- Declaration des sorties --
type(st_genericfield)        :: flux
real(krp), dimension(nflux)  :: jacL, jacR  ! jac associees

! -- Declaration des variables internes --
real(krp), parameter      :: theta = 1._krp
real(krp), dimension(taille_buffer) :: dHR, dHL   ! cell to face distance
type(v3d), dimension(taille_buffer) :: vLR        ! cell to cell vector
type(v3d), dimension(taille_buffer) :: vectH      ! face (H) vector (velocity or temperature gradient)
type(t3d), dimension(taille_buffer) :: velH       ! face (H) velocity
real(krp), dimension(taille_buffer) :: TH, mu     ! temperature en H
real(krp)  :: id
integer    :: if

! -- Debut de la procedure --

!allocate( kH(nflux))    ! conductivite en H, centre de face
!allocate(dHR(nflux))    ! distance HR, rapportee a HL+HR
!allocate(dHL(nflux))    ! distance HL, rapportee a HL+HR
!allocate(dLR(nflux))    ! distance LR (difference de HR+HL)
!allocate(vLR(nflux))    ! vecteur  LR

! -- Calculs preliminaires --

do if = 1, nflux
  dHL(if) = abs(face(if)%centre - cg_l(if))
  dHR(if) = abs(face(if)%centre - cg_r(if))
  id      = 1._krp/(dHL(if) + dHR(if))
  dHL(if) = id*dHL(if) 
  dHR(if) = id*dHR(if) 
  vLR(if) = cg_r(if) - cg_l(if)
  ! DEV / OPT : calcul de la distance au carree si c'est la seule utilisee
  ! pour eviter sqrt()**2
  !dLR(if) = abs(vLR(if))
enddo

! -- Calcul de la conductivite en H (centre de face) selon le materiau --

select case(defsolver%defkdif%materiau%type)

case(mat_LIN)
  kH(:) = defsolver%defkdif%materiau%Kd%valeur

case(mat_KNL)
  do if = 1, nflux
    TH     = dHR(if)*cell_l(if)%temperature + dHL(if)*cell_r(if)%temperature
    kH(if) = valeur_loi(defsolver%defkdif%materiau%Kd, TH)
  enddo

case(mat_XMAT)
  call erreur("Calcul de materiau","Materiau non lineaire complet interdit")

endselect

!--------------------------------------------------------------
! Calcul du flux
!--------------------------------------------------------------
! COMPACT : F1 = - k(H) * (T(R) - T(L))            ! L et R centres de cellules
! AVERAGE : F2 = - k(H) * (a.gT(L) + b.gT(R)).n    ! H centre de face
! FULL    : F3 = 
! a = HR/RL et b = HL/RL
! k(H) = k(T(H)) avec T(H) = a.T(L) + b.T(R)

select case(defspat%sch_dis)
case(dis_dif2) ! formulation compacte, non consistance si vLR et n non alignes
  do if = 1, nflux
    flux(if,1)  = - kH(if) * (cell_r(if)%temperature - cell_l(if)%temperature) &
                           * (vLR(if).scal.face(if)%normale) / (dLR(if)**2)
  enddo

case(dis_avg2) ! formulation consistante, moyenne ponderee des gradients
  do if = 1, nflux
    flux(if,1)  = - kH(if) * ((dHL(if)*grad_r(if) + dHR(if)*grad_l(if)).scal.face(if)%normale)
  enddo

case(dis_full)
  do if = 1, nflux
    pscal = (vLR(if).scal.face(if)%normale) / (dLR(if)**2)
    Fcomp = pscal * (cell_r(if)%temperature - cell_l(if)%temperature)
    vi    = face(if)%normale - (theta*pscal)*vLR(if)
    Favg  = (dHL(if)*grad_r(if) + dHR(if)*grad_l(if)).scal.vi
    flux(if,1)  = - kH(if) * (theta*Fcomp + Favg)
  enddo

endselect

! -- Calculs preliminaires --

do i = 1, nflux

enddo

!--------------------------------------------------------------
! Calcul des jacobiennes
!--------------------------------------------------------------
if (calc_jac) then
  call erreur("Developpement","Calcul de jacobiennes du flux VISCOUS non implemente")
endif
!  do if = 1, nflux
!    jacR(if) =  - kH(if) * (vLR(if).scal.face(if)%normale) &
!                  / (defsolver%defkdif%materiau%Cp * dLR(if)**2)
!    jacL(if) = -jacR(if)
!  enddo
!endif


!deallocate()


endsubroutine calc_flux_viscous

!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2005 : creation, VISCOUS flux
!------------------------------------------------------------------------------!
