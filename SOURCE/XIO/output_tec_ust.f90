!------------------------------------------------------------------------------!
! Procedure : output_tec_ust              Auteur : J. Gressier
!                                         Date   : Décembre 2002
! Fonction                                Modif  : 
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format TECPLOT
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_tec_ust(uf, ust_mesh, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrées --
integer          :: uf            ! unité d'écriture
type(st_ustmesh) :: ust_mesh      ! maillage à écrire
type(st_field)   :: field         ! champ de valeurs

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i
integer   :: info
type(v3d) :: vtex

real(krp) :: a, b, L, T0, T1, alpha, beta, temp

! -- Debut de la procedure --

write(uf_chpresu,*) 'ZONE T="USTMESH"' !, F=FEPOINT, N=',ust_mesh%nvtex,',E=',ncell

! attention : il faut recalculer les points au sommets ou
! écrire le maillage des centres de cellule

do i = 1, ust_mesh%ncell
!do i = 1, ust_mesh%ncell_int
  vtex = ust_mesh%mesh%centre(i,1,1)

  write(uf_chpresu,'(4e18.8)') vtex%x, vtex%y, vtex%z, field%etatprim%tabscal(1)%scal(i)

enddo

! calcul de la connectivité sommets -> sommets

!do i = 1, ust_mesh%ncell
!  vtex = ust_mesh%mesh%vertex(i)
!  write(uf_chpresu,'(4e15.8)') vtex%x, vtex%y, vtex%z, 1._krp
!enddo


endsubroutine output_tec_ust
