!------------------------------------------------------------------------------!
! Procedure : extract_points              Auteur : J. Gressier / E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Extraction de la liste des centres de face d'une condition aux limites 
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine extract_centre(boco, umesh, centre)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --
type(st_ustboco) :: boco
type(st_ustmesh) :: umesh

! -- Declaration des entrees/sorties --
type(v3d), dimension(1:boco%nface) :: centre

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: if, iface

! -- Debut de la procedure --

do if = 1, boco%nface
  iface = boco%iface(if)
  centre(if) = umesh%mesh%iface(iface,1,1)%centre
enddo

endsubroutine extract_centre

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Fevrier 2004 : creation de la procedure
! 
!------------------------------------------------------------------------------!
