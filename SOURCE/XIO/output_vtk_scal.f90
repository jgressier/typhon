!-----------------------------------------------------------------------------!
! Procedure : output_vtk_scal             Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs SCALAIRES
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtk_scal(uf, ust_mesh, name, scafld)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrées --
integer           :: uf            ! unité d'écriture
type(st_ustmesh)  :: ust_mesh      ! maillage à écrire
type(st_scafield) :: scafld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i

! -- Debut de la procedure --

write(uf,'(a)')    'SCALARS '//trim(name)//' float'
write(uf,'(a)')    'LOOKUP_TABLE default'

do i = 1, ust_mesh%cellvtex%nbar
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%ibar(i))
enddo
do i = 1, ust_mesh%cellvtex%ntri
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%itri(i))
enddo
do i = 1, ust_mesh%cellvtex%nquad
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%iquad(i))
enddo
do i = 1, ust_mesh%cellvtex%ntetra
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%itetra(i))
enddo
do i = 1, ust_mesh%cellvtex%npyra
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%ipyra(i))
enddo
do i = 1, ust_mesh%cellvtex%npenta
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%ipenta(i))
enddo
do i = 1, ust_mesh%cellvtex%nhexa
  write(uf,'(e18.8)') scafld%scal(ust_mesh%cellvtex%ihexa(i))
enddo


endsubroutine output_vtk_scal

!------------------------------------------------------------------------------!
! Historique des modifications
!
! July 2004 : subroutine creation, from output_vtk_cell
!------------------------------------------------------------------------------!
