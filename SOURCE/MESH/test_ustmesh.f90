!------------------------------------------------------------------------------!
! Procedure : test_ustmesh               Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  :
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

! -- Declaration des entrées --

! -- Declaration des entrées/sorties --
type(st_ustmesh) :: ust_mesh

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --

! allocation 

!ust_mesh%mesh%nface = ust_mesh%nface                  ! copie du nombre de faces
!allocate(ust_mesh%mesh%iface(ust_mesh%nface,1,1))     ! allocation des faces
!allocate(cgface(ust_mesh%nface))                      ! tab. interm. centre G des faces



endsubroutine test_ustmesh
