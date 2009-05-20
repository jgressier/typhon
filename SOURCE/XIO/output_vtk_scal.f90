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

subroutine output_vtk_scal(uf, umesh, name, scafld)

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
type(st_scafield) :: scafld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i, ielem

! -- BODY --

write(uf,'(A)')    'SCALARS '//trim(name)//' double'
write(uf,'(A)')    'LOOKUP_TABLE default'

do ielem = 1, umesh%cellvtex%ntype

  do i = 1, umesh%cellvtex%elem(ielem)%nelem
    write(uf,'(1P,E17.8E3)') scafld%scal(umesh%cellvtex%elem(ielem)%ielem(i))
  enddo

enddo

endsubroutine output_vtk_scal

!------------------------------------------------------------------------------!
! Changes history
!
! July 2004 : subroutine creation, from output_vtk_cell
!------------------------------------------------------------------------------!
