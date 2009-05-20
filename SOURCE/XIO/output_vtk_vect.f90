!-----------------------------------------------------------------------------!
! Procedure : output_vtk_vect                   Auteur : J. Gressier
!                                               Date   : July 2004
! Function                                      Modif  : (cf historique)
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules / Ecriture de champs VECTORIELS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtk_vect(uf, umesh, name, vecfld)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use USTMESH
use DEFFIELD

implicit none

! -- Declaration des entrees --
integer           :: uf            ! unite d'ecriture
type(st_ustmesh)  :: umesh         ! unstructured mesh
type(st_vecfield) :: vecfld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i, ielem

! -- BODY --

write(uf,'(A)')    'VECTORS '//trim(name)//' double'

do ielem = 1, umesh%cellvtex%ntype

  do i = 1, umesh%cellvtex%elem(ielem)%nelem
    write(uf,'(:,1P,3E17.8E3)') vecfld%vect(umesh%cellvtex%elem(ielem)%ielem(i))
  enddo

enddo

endsubroutine output_vtk_vect

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : subroutine creation, from output_vtk_cell
!------------------------------------------------------------------------------!
