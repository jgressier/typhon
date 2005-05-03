!------------------------------------------------------------------------------!
! Procedure : calcul_raccord              Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!   Calcul des donnees d'un raccord : indices de couplage et de conditions aux
!   limites pour chaque zone couplee 
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
type(st_world) :: lworld
integer        :: ir               ! indice du couplage

! -- Declaration des entrees/sorties --
integer        :: iz1, iz2         ! indices des zones
integer        :: ncoupl1, ncoupl2 ! indices de raccord pour les zones couplees
integer        :: nbc1, nbc2       ! indices de CL pour les zones couplees

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: ic, ib                ! index de couplage et de conditions limites

! -- Debut de la procedure --

iz1 = lworld%coupling(ir)%zone1
iz2 = lworld%coupling(ir)%zone2

! Determination des numeros du raccord pour les zones 1 et 2
do ic = 1, lworld%zone(iz1)%ncoupling
  if (samestring(lworld%zone(iz1)%coupling(ic)%connzone, lworld%zone(iz2)%name)) then
    ncoupl1 = ic
  endif
enddo

do ic = 1, lworld%zone(iz2)%ncoupling
  if (samestring(lworld%zone(iz2)%coupling(ic)%connzone, lworld%zone(iz1)%name)) then
    ncoupl2 = ic
  endif
enddo


! Determination des indices de condition aux limites pour les zones 1 et 2
do ib = 1, lworld%zone(iz1)%grid%umesh%nboco
  if (samestring(lworld%zone(iz1)%coupling(ncoupl1)%family, &
                 lworld%zone(iz1)%grid%umesh%boco(ib)%family)) then
    nbc1 = ib
  endif
enddo
  
do ib = 1, lworld%zone(iz2)%grid%umesh%nboco
  if (samestring(lworld%zone(iz2)%coupling(ncoupl2)%family, &
                 lworld%zone(iz2)%grid%umesh%boco(ib)%family)) then
    nbc2 = ib
  endif
enddo

endsubroutine calcul_raccord

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
!------------------------------------------------------------------------------!
