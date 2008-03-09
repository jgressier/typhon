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

subroutine output_tec_ust_ctr(uf, ust_mesh, field, defsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
integer          :: uf            ! unite d'ecriture
type(st_ustmesh) :: ust_mesh      ! maillage a ecrire
type(st_field)   :: field         ! champ de valeurs
type(mnu_solver) :: defsolver     ! solver parameters

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i
integer   :: info
type(v3d) :: vtex
real(krp) :: temperature

! -- Debut de la procedure --

write(uf,*) 'ZONE T="USTMESH"' !, F=FEPOINT, N=',ust_mesh%nvtex,',E=',ncell

! attention : il faut recalculer les points au sommets ou
! ecrire le maillage des centres de cellule

do i = 1, ust_mesh%ncell
  vtex = ust_mesh%mesh%centre(i,1,1)

  select case(defsolver%typ_solver)

    case(solKDIF)
    write(uf,'(4e18.8)') vtex%x, vtex%y, vtex%z, field%etatprim%tabscal(1)%scal(i)

    case(solNS)
    temperature = field%etatprim%tabscal(2)%scal(i) / &
                  ( field%etatprim%tabscal(1)%scal(i) * &
                  defsolver%defns%properties(1)%r_const )
    write(uf,'(8e18.8)') vtex%x, vtex%y, vtex%z, &
                         field%etatprim%tabvect(1)%vect(i)%x, &
                         field%etatprim%tabvect(1)%vect(i)%y, &
                         field%etatprim%tabvect(1)%vect(i)%z, &
                         field%etatprim%tabscal(2)%scal(i), &
                         temperature

  endselect

enddo

! calcul de la connectivite sommets -> sommets

!do i = 1, ust_mesh%ncell
!  vtex = ust_mesh%mesh%vertex(i)
!  write(uf_chpresu,'(4e15.8)') vtex%x, vtex%y, vtex%z, 1._krp
!enddo


endsubroutine output_tec_ust_ctr
