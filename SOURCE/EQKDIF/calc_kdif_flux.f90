!------------------------------------------------------------------------------!
! Procedure : calc_kdif_flux              Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des flux de conduction de la chaleur : trois méthodes
!
! Defauts/Limitations/Divers :
!   Les gradients sont censés être ceux des variables primitives qui
!   sont aussi passées en argument
!
!------------------------------------------------------------------------------!
subroutine calc_kdif_flux(defsolver, defspat, nflux, face,   &
                          cg_l, cell_l, grad_l,              &
                          cg_r, cell_r, grad_r, flux)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQKDIF
use GEO3D
use MATER_LOI

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: defsolver        ! paramètres de définition du solveur
type(mnu_spat)        :: defspat          ! paramètres d'intégration spatiale
integer               :: nflux            ! nombre de flux (face) à calculer
type(st_face),     dimension(1:nflux) & 
                      :: face             ! données géométriques des faces
type(v3d),         dimension(1:nflux) &
                      :: cg_l, cg_r       ! centres des cellules
type(st_kdifetat), dimension(1:nflux) &
                      :: cell_l, cell_r   ! champs des valeurs primitives
type(v3d),         dimension(1:nflux) &
                      :: grad_l, grad_r   ! gradients aux centres des cellules

! -- Declaration des sorties --
real(krp), dimension(nflux, defsolver%nequat) :: flux

! -- Declaration des variables internes --
real(krp), parameter :: theta = 1._krp
integer   :: if
real(krp), dimension(:), allocatable :: kH, dHR, dHL, dLR  ! voir allocation
type(v3d), dimension(:), allocatable :: vLR                ! vecteur entre centres de cellules
real(krp) :: TH                                            ! température en H
real(krp) :: Fcomp, Favg                                   ! flux compacts et moyens
real(krp) :: id, pscal                                     ! reéls temporaires
type(v3d) :: vi                                            ! vecteur intermédiaire

! -- Debut de la procedure --

allocate( kH(nflux))    ! conductivité en H, centre de face
allocate(dHR(nflux))    ! distance HR, rapportée à HL+HR
allocate(dHL(nflux))    ! distance HL, rapportée à HL+HR
allocate(dLR(nflux))    ! distance LR (différence de HR+HL)
allocate(vLR(nflux))    ! vecteur  LR

! -- Calculs préliminaires --

do if = 1, nflux
  dHL(if) = abs(face(if)%centre - cg_l(if))
  dHR(if) = abs(face(if)%centre - cg_r(if))
  id      = 1._krp/(dHL(if) + dHR(if))
  dHL(if) = id*dHL(if) 
  dHR(if) = id*dHR(if) 
  vLR(if) = cg_r(if) - cg_l(if)
  ! DEV / OPT : calcul de la distance au carrée si c'est la seule utilisée
  ! pour éviter sqrt()**2
  dLR(if) = abs(vLR(if))
enddo

! -- Calcul de la conductivité en H (centre de face) selon le matériau --

select case(defsolver%defkdif%materiau%type)

case(mat_LIN)
  kH(:) = defsolver%defkdif%materiau%Kd%valeur

case(mat_KNL)
  do if = 1, nflux
    TH     = dHR(if)*cell_l(if)%temperature + dHL(if)*cell_r(if)%temperature
    kH(if) = valeur_loi(defsolver%defkdif%materiau%Kd, TH)
  enddo

case(mat_XMAT)
  call erreur("Calcul de matériau","Materiau non linéaire complet interdit")

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
case(dis_dif2) ! formulation compacte, non consistance si vLR et n non alignés
  do if = 1, nflux
    flux(if,1)  = - kH(if) * (cell_r(if)%temperature - cell_l(if)%temperature) &
                           * (vLR(if).scal.face(if)%normale) / (dLR(if)**2)
  enddo

case(dis_avg2) ! formulation consistante, moyenne pondérée des gradients
  do if = 1, nflux
    flux(if,1)  = - kH(if) * ((dHL(if)*grad_r(if) + dHR(if)*grad_l(if)).scal.face(if)%normale)
  enddo

case(dis_full)
  do if = 1, nflux
    pscal = (vLR(if).scal.face(if)%normale) / (dLR(if)**2)
    Fcomp = pscal * (cell_r(if)%temperature - cell_l(if)%temperature)
    vi    = face(if)%normale - (theta*pscal)*vLR(if)
    Favg  = (dHL(if)*grad_r(if) + dHR(if)*grad_l(if)).scal.vi
    !!write(*,"(a,4e12.4)") "DEBUG: ",Favg, Fcomp, abs(grad_l(if)), abs(grad_r(if))
    flux(if,1)  = - kH(if) * (theta*Fcomp + Favg)
  enddo

endselect

deallocate(kH, dHR, dHL, dLR, vLR)


endsubroutine calc_kdif_flux

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003  : création de la procédure : méthode COMPACTE
! juil 2003  : conductivité non constante
! sept 2003  : optimisation de la procédure pour récupérer les temps CPU initiaux
! oct  2003  : implémentation des trois méthodes de calcul COMPACT, AVERAGE, FULL
!------------------------------------------------------------------------------!
