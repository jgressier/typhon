!------------------------------------------------------------------------------!
! Procedure : init_maillage               Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Calcul et initialisation du maillage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_maillage(zone)

use TYPHMAKE
use VARCOM
use OUTPUT
use DEFZONE

implicit none

! -- Declaration des entrées --

! -- Declaration des entrées/sorties --
type(st_zone) :: zone

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(zone%typ_mesh)

case(mshSTR)
  call erreur("Dévéloppement (init_maillage)", &
              "maillage structuré non implémenté")
  !do i = 1, zone%str_mesh%nblock
  !  call integration_strdomaine(dt, zone%typ_solver, zone%str_mesh%block(i))
  !enddo

case(mshUST)
  call calc_ustmesh(zone%ust_mesh)

case default
  call erreur("incohérence interne (init_maillage)", &
              "type de maillage inconnu")

endselect


endsubroutine init_maillage
