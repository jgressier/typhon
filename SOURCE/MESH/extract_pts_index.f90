!------------------------------------------------------------------------------!
! Procedure : extract_pts_index           Auteur : J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Creation d'une liste d'index des points presents dans une famille BOCO
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine extract_pts_index(umesh, boco, npts, liste)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --
type(st_ustmesh) :: umesh
type(st_ustboco) :: boco      ! condition aux limite et liste des faces concernees

! -- Declaration des entrees/sorties --

! -- Declaration des sorties --
integer                           :: npts      ! nombre de points inclus dans la famille
integer, dimension(1:umesh%nvtex) :: liste     ! liste des index des points renumerotes

! -- Declaration des variables internes --
integer :: if, iface, iv, ivtex

! -- Debut de la procedure --


liste(:) = 0     ! initialisation
npts     = 0

do if = 1, boco%nface
  iface = boco%iface(if)
  do iv = 1, umesh%facevtex%nbfils
    ivtex = umesh%facevtex%fils(iface,iv)
    if (ivtex /= 0) then            ! si le sommet de la face est defini
      if (liste(ivtex) == 0) then   ! si le sommet n'a pas ete ajoute dans la liste
        npts = npts + 1               ! on ajoute le sommet a la liste
        liste(ivtex) = npts           ! et on le renumerote (dans l'ordre d'apparition)
      endif
    endif
  enddo
enddo


endsubroutine extract_pts_index

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juin 2003 (v0.0.1b): creation de la procedure
! 
!------------------------------------------------------------------------------!
