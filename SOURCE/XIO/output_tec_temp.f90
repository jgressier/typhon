!------------------------------------------------------------------------------!
! Procedure : output_tec_temp             Auteur : E. Radenac / J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf Historique)
!   Affichage au format tecplot des temperatures a l'interface
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine output_tec_temp(nom, lworld, io)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: lworld
integer               :: io        ! indice de la sortie

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: izone, ir, i
integer   :: iz1, iz2, ncoupl1, ncoupl2,  nbc1, nbc2

! -- Debut de la procedure --
if (lworld%info%curtps == 0) then
  open(unit = uf_tempinter, file = trim(lworld%output(io)%fichier), form = 'formatted')
  write(uf_tempinter, '(a)') 'VARIABLES="t","Tw"'
endif

! Calcul des conditions aux limites pour le calcul des flux a l'interface

do izone = 1, lworld%prj%nzone
 call conditions_limites(lworld%zone(izone))
enddo

if (lworld%prj%ncoupling > 0) then

ir =1 ! DVT : provisoire
    
! calcul des donnees de raccord : indices de raccord, de CL pour les 
! deux zones couplees
call calcul_raccord(lworld, ir, iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2)

do i = 1, lworld%zone(iz1)%grid%umesh%boco(nbc1)%nface

  write(uf_tempinter,'(2e18.8)') lworld%info%curtps, &
    lworld%zone(iz1)%defsolver%boco(lworld%zone(iz1)%grid%umesh%boco(nbc1)%idefboco)%boco_kdif%temp(i)
                  
enddo

endif

endsubroutine output_tec_temp

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr 2004   : creation de la procedure
!------------------------------------------------------------------------------!
