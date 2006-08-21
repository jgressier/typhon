!-----------------------------------------------------------------------------!
! Procedure : output_vtkbin_vect             Auteur : J. Gressier
!                                            Date   : April 2006
! Fonction                  
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs VECLAIRES
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

! -- Debut de la procedure --

call writereturn(uf)
call writestr(uf, 'VECTORS '//trim(name)//' float')

do i = 1, ust_mesh%cellvtex%nbar
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%ibar(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%ntri
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%itri(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%nquad
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%iquad(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%ntetra
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%itetra(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%npyra
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%ipyra(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%npenta
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%ipenta(i))), kind=4)
enddo
do i = 1, ust_mesh%cellvtex%nhexa
  write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%ihexa(i))), kind=4)
enddo


endsubroutine output_vtkbin_vect

!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
!------------------------------------------------------------------------------!
