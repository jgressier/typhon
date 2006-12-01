!-----------------------------------------------------------------------------!
! Procedure : output_vtk_scal                   Auteur : J. Gressier
!                                               Date   : July 2004
! Function                                      Modif  : (cf historique)
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

! -- Declaration des entrees --
integer           :: uf            ! unite d'ecriture
type(st_ustmesh)  :: ust_mesh      ! maillage a ecrire
type(st_scafield) :: scafld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i
type(st_cellvtex) :: cvtx

! -- Debut de la procedure --

write(uf,'(A)')    'SCALARS '//trim(name)//' double'
write(uf,'(A)')    'LOOKUP_TABLE default'

cvtx = ust_mesh%cellvtex

do i = 1, cvtx%nbar
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%ibar(i))
enddo
do i = 1, cvtx%ntri
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%itri(i))
enddo
do i = 1, cvtx%nquad
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%iquad(i))
enddo
do i = 1, cvtx%ntetra
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%itetra(i))
enddo
do i = 1, cvtx%npyra
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%ipyra(i))
enddo
do i = 1, cvtx%npenta
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%ipenta(i))
enddo
do i = 1, cvtx%nhexa
  write(uf,'(1P,E17.8E3)') scafld%scal(cvtx%ihexa(i))
enddo

endsubroutine output_vtk_scal

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : subroutine creation, from output_vtk_cell
!------------------------------------------------------------------------------!
