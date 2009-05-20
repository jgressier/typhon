!------------------------------------------------------------------------------!
! Procedure : test_ustmesh                Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf historique)
!   Test de la structure pour permettre l'initialisation et le calcul du maillage
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine test_ustmesh(umesh)

use TYPHMAKE
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --

! -- Declaration des entrees/sorties --
type(st_ustmesh) :: umesh

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --


!umesh%mesh%nface = umesh%nface                  ! copie du nombre de faces
!allocate(umesh%mesh%iface(umesh%nface,1,1))     ! allocation des faces
!allocate(cgface(umesh%nface))                   ! tab. interm. centre G des faces



endsubroutine test_ustmesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : creation de la procedure (vide)
! mars 2004 : test minimaux d'existence de maillage
!------------------------------------------------------------------------------!
