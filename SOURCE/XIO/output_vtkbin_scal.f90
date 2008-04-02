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
integer   :: i, ielem

! -- BODY --

call writestr(uf, 'SCALARS '//trim(name)//' double')
call writestr(uf, 'LOOKUP_TABLE default')

do ielem = 1, ust_mesh%cellvtex%ntype

  do i = 1, ust_mesh%cellvtex%elem(ielem)%nelem
    write(uf) real(scafld%scal(ust_mesh%cellvtex%elem(ielem)%ielem(i)), kind=8)
  enddo

enddo

call writereturn(uf)

endsubroutine output_vtkbin_scal

!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
!------------------------------------------------------------------------------!
