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
integer   :: i, ielem

! -- BODY --

call writestr(uf, 'VECTORS '//trim(name)//' double')

do ielem = 1, ust_mesh%cellvtex%ntype

  do i = 1, ust_mesh%cellvtex%elem(ielem)%nelem
    write(uf) real(tab(vecfld%vect(ust_mesh%cellvtex%elem(ielem)%ielem(i))), kind=8)
  enddo

enddo

call writereturn(uf)

endsubroutine output_vtkbin_vect

!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
!------------------------------------------------------------------------------!
