!------------------------------------------------------------------------------!
! Procedure : test_ustmesh                Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Test de la structure pour permettre l'initialisation et le calcul du maillage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine test_ustmesh(ust_mesh)

use TYPHMAKE
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --

! -- Declaration des entrees/sorties --
type(st_ustmesh) :: ust_mesh

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --


!ust_mesh%mesh%nface = ust_mesh%nface                  ! copie du nombre de faces
!allocate(ust_mesh%mesh%iface(ust_mesh%nface,1,1))     ! allocation des faces
!allocate(cgface(ust_mesh%nface))                      ! tab. interm. centre G des faces



endsubroutine test_ustmesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : creation de la procedure (vide)
! mars 2004 : test minimaux d'existence de maillage
!------------------------------------------------------------------------------!
