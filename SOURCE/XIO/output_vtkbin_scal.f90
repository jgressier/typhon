!-----------------------------------------------------------------------------!
! Procedure : output_vtkbin_scal                Auteur : J. Gressier
!                                               Date   : April 2006
! Function
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs SCALAIRES
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtkbin_scal(uf, ust_mesh, name, scafld)

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

call writestr(uf, 'SCALARS '//trim(name)//' double')
call writestr(uf, 'LOOKUP_TABLE default')

cvtx = ust_mesh%cellvtex

do i = 1, cvtx%nbar
  write(uf) real(scafld%scal(cvtx%ibar(i)), kind=8)
enddo
do i = 1, cvtx%ntri
  write(uf) real(scafld%scal(cvtx%itri(i)), kind=8)
enddo
do i = 1, cvtx%nquad
  write(uf) real(scafld%scal(cvtx%iquad(i)), kind=8)
enddo
do i = 1, cvtx%ntetra
  write(uf) real(scafld%scal(cvtx%itetra(i)), kind=8)
enddo
do i = 1, cvtx%npyra
  write(uf) real(scafld%scal(cvtx%ipyra(i)), kind=8)
enddo
do i = 1, cvtx%npenta
  write(uf) real(scafld%scal(cvtx%ipenta(i)), kind=8)
enddo
do i = 1, cvtx%nhexa
  write(uf) real(scafld%scal(cvtx%ihexa(i)), kind=8)
enddo
call writereturn(uf)

endsubroutine output_vtkbin_scal

!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
!------------------------------------------------------------------------------!
