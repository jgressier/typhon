!------------------------------------------------------------------------------!
! Procedure : output_tecplot_ust          Auteur : J. Gressier
!                                         Date   : Decembre 2002
! Fonction                                Modif  : Octobre 2003
!   Ecriture fichier des champs de chaque zone au format TECPLOT
!   Choix entre sortie des valeurs aux noeuds ou aux centres des cellules
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_tec_ust(uf, ust_mesh, field, outp_typ, defsolver)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
integer          :: uf            ! unite d'ecriture
type(st_ustmesh) :: ust_mesh      ! maillage a ecrire
type(st_field)   :: field         ! champ de valeurs
integer          :: outp_typ      ! type de sortie
type(mnu_solver) :: defsolver     ! solver parameters

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --
select case(outp_typ)

case(outp_NODE) ! Sortie des valeurs aux noeuds du maillage
  call output_tec_ust_node(uf, ust_mesh, field)

case(outp_CENTER) ! Sortie des valeurs aux centres des cellules
  call output_tec_ust_ctr(uf, ust_mesh, field, defsolver)

end select

endsubroutine output_tec_ust
