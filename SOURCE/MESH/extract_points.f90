!------------------------------------------------------------------------------!
! Procedure : extract_points              Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Extraction d'une liste de points à partir d'une liste d'index
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine extract_points(umesh, indx, liste)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrées --
type(st_ustmesh) :: umesh
integer, dimension(1:umesh%nvtex) :: indx     ! liste des index des points renumérotés

! -- Declaration des entrées/sorties --

! -- Declaration des sorties --
type(v3d), dimension(*) :: liste  ! liste des sommets à extraire

! -- Declaration des variables internes --
integer :: if, iface, iv, nvtex

! -- Debut de la procedure --

!! instruction where ?

do iv = 1, umesh%nvtex
  if (indx(iv) /= 0) then
    liste(indx(iv)) = umesh%mesh%vertex(iv,1,1)
  endif
enddo


endsubroutine extract_points

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juin 2003 (v0.0.1b): création de la procédure
! 
!------------------------------------------------------------------------------!
