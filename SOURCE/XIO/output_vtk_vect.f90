!-----------------------------------------------------------------------------!
! Procedure : output_vtk_vect             Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs VECLAIRES
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtk_vect(uf, ust_mesh, name, vecfld)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
integer           :: uf            ! unite d'ecriture
type(st_ustmesh)  :: ust_mesh      ! maillage a ecrire
type(st_vecfield) :: vecfld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i

! -- Debut de la procedure --

write(uf,'(a)')    'VECTORS '//trim(name)//' float'

do i = 1, ust_mesh%cellvtex%nbar
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%ibar(i))
enddo
do i = 1, ust_mesh%cellvtex%ntri
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%itri(i))
enddo
do i = 1, ust_mesh%cellvtex%nquad
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%iquad(i))
enddo
do i = 1, ust_mesh%cellvtex%ntetra
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%itetra(i))
enddo
do i = 1, ust_mesh%cellvtex%npyra
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%ipyra(i))
enddo
do i = 1, ust_mesh%cellvtex%npenta
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%ipenta(i))
enddo
do i = 1, ust_mesh%cellvtex%nhexa
  write(uf,'(3e18.8)') vecfld%vect(ust_mesh%cellvtex%ihexa(i))
enddo


endsubroutine output_vtk_vect

!------------------------------------------------------------------------------!
! Historique des modifications
!
! July 2004 : subroutine creation, from output_vtk_cell
!------------------------------------------------------------------------------!
