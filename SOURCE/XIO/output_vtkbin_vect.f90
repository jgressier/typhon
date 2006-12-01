!-----------------------------------------------------------------------------!
! Procedure : output_vtkbin_vect                Auteur : J. Gressier
!                                               Date   : April 2006
! Function
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs VECTORIELS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtkbin_vect(uf, ust_mesh, name, vecfld)

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
type(st_cellvtex) :: cvtx

! -- Debut de la procedure --

call writestr(uf, 'VECTORS '//trim(name)//' double')

cvtx = ust_mesh%cellvtex

do i = 1, cvtx%nbar
  write(uf) real(tab(vecfld%vect(cvtx%ibar(i))), kind=8)
enddo
do i = 1, cvtx%ntri
  write(uf) real(tab(vecfld%vect(cvtx%itri(i))), kind=8)
enddo
do i = 1, cvtx%nquad
  write(uf) real(tab(vecfld%vect(cvtx%iquad(i))), kind=8)
enddo
do i = 1, cvtx%ntetra
  write(uf) real(tab(vecfld%vect(cvtx%itetra(i))), kind=8)
enddo
do i = 1, cvtx%npyra
  write(uf) real(tab(vecfld%vect(cvtx%ipyra(i))), kind=8)
enddo
do i = 1, cvtx%npenta
  write(uf) real(tab(vecfld%vect(cvtx%ipenta(i))), kind=8)
enddo
do i = 1, cvtx%nhexa
  write(uf) real(tab(vecfld%vect(cvtx%ihexa(i))), kind=8)
enddo
call writereturn(uf)

endsubroutine output_vtkbin_vect

!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
!------------------------------------------------------------------------------!
