!------------------------------------------------------------------------------!
! Procedure : init_coupling_kdif          Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juillet 2003
!   Initialisation des structures de données d'échange et de résultats de
!   couplage pour la thermique
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_coupling_kdif(coupling, nface)

use TYPHMAKE
use MENU_ZONECOUPLING
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrées --
integer                :: nface

! -- Declaration des sorties --

! -- Declaration des entrées/sorties --
type(mnu_zonecoupling) :: coupling

! -- Declaration des variables internes --
type(v3d)              :: vnul ! vecteur nul
integer                :: i

! -- Debut de la procedure --

! Vecteur nul
  vnul%x = 0._krp
  vnul%y = 0._krp
  vnul%z = 0._krp

  call new(coupling%zcoupling%echdata, nface, 2, 1, 0)
  call new(coupling%zcoupling%etatcons, nface, 2, 0, 0)

  call init_genericfield(coupling%zcoupling%echdata, 0._krp, vnul)
  call init_genericfield(coupling%zcoupling%etatcons, 0._krp, vnul)

endsubroutine init_coupling_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création du module
! juillet 2003       : ajout des manipulations sur etatcons
!------------------------------------------------------------------------------!
