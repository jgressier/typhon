!-----------------------------------------------------------------------------!
! Procedure : output_tec_ust_ctr          Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : 
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format TECPLOT
!   Valeurs au centre des cellules
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_tec_ust_ctr(uf, ust_mesh, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
integer          :: uf            ! unite d'ecriture
type(st_ustmesh) :: ust_mesh      ! maillage a ecrire
type(st_field)   :: field         ! champ de valeurs

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i
integer   :: info
type(v3d) :: vtex

! -- Debut de la procedure --
write(uf,*) 'ZONE T="USTMESH"' !, F=FEPOINT, N=',ust_mesh%nvtex,',E=',ncell

! attention : il faut recalculer les points au sommets ou
! ecrire le maillage des centres de cellule

do i = 1, ust_mesh%ncell
  vtex = ust_mesh%mesh%centre(i,1,1)
  write(uf,'(4e18.8)') vtex%x, vtex%y, vtex%z, field%etatprim%tabscal(1)%scal(i)
enddo

! calcul de la connectivite sommets -> sommets

!do i = 1, ust_mesh%ncell
!  vtex = ust_mesh%mesh%vertex(i)
!  write(uf_chpresu,'(4e15.8)') vtex%x, vtex%y, vtex%z, 1._krp
!enddo


endsubroutine output_tec_ust_ctr
