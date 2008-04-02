!-----------------------------------------------------------------------------!
! Procedure : output_vtk_cell                   Auteur : J. Gressier
!                                               Date   : Avril 2004
! Function                                      Modif  : (cf historique)
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules
!
!
!------------------------------------------------------------------------------!

subroutine output_vtk_cell(uf, defsolver, ust_mesh, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use MENU_SOLVER
use USTMESH
use DEFFIELD

implicit none

! -- INPUTS --
integer         , intent(in) :: uf            ! unite d'ecriture
type(mnu_solver), intent(in) :: defsolver     ! parametres du solveur
type(st_ustmesh), intent(in) :: ust_mesh      ! maillage a ecrire
type(st_field),   intent(in) :: field         ! champ de valeurs

! -- OUTPUTS --

! -- Internal variables --
integer   :: i, ncellint
integer   :: info, ntot
type(v3d) :: vtex
integer   :: ielem, nvtex, vtktype

! -- BODY --


! ecriture du maillage

write(uf,'(A)')      'DATASET UNSTRUCTURED_GRID'
write(uf,'(A,I9,A)') 'POINTS ', ust_mesh%nvtex, ' double'

! coordonnees

do i = 1, ust_mesh%nvtex
  vtex = ust_mesh%mesh%vertex(i,1,1)
  write(uf,'(1P,4E17.8E3)') vtex%x, vtex%y, vtex%z
enddo

! CELLVTEX connectivity

ncellint       = ust_mesh%ncell_int
ntot           = 0                       ! number of written integer in CELLVTEX connectivity

do ielem = 1, ust_mesh%cellvtex%ntype
  nvtex = ust_mesh%cellvtex%elem(ielem)%nvtex
  ntot  = ntot + (nvtex+1)*ust_mesh%cellvtex%elem(ielem)%nelem
enddo

write(uf,'(A,2i10)') 'CELLS ', ncellint, ntot

do ielem = 1, ust_mesh%cellvtex%ntype

  nvtex = ust_mesh%cellvtex%elem(ielem)%nvtex
  do i = 1, ust_mesh%cellvtex%elem(ielem)%nelem
    write(uf,'(i3,2I9)') nvtex, ust_mesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
  enddo

enddo

! type de cellules

write(uf,'(A,I9)') 'CELL_TYPES ', ncellint

do ielem = 1, ust_mesh%cellvtex%ntype

  select case(ust_mesh%cellvtex%elem(ielem)%elemtype)
  case(elem_bar2)
    vtktype = 3    ! VTK_LINE 
  case(elem_tri3)
    vtktype = 5    ! VTK_TRIANGLE
  case(elem_quad4)
    vtktype = 9    ! VTK_QUAD
  case(elem_tetra4)
    vtktype = 10   ! VTK_TETRA
  case(elem_pyra5)
    vtktype = 14   ! VTK_PYRAMID
  case(elem_penta6)
    vtktype = 13   ! VTK_WEDGE
  case(elem_hexa8)
    vtktype = 12   ! VTK_HEXAHEDRON
  case default
    call erreur("VTK writer", "do not known how to write this element type")
  endselect

  write(uf,'(I2)') vtktype

enddo

! -- donnees --

call calc_varprim(defsolver, field)

select case(defsolver%typ_solver)
case(solNS)
  write(uf,'(A,I9)') 'CELL_DATA ', ncellint
  call output_vtk_scal(uf, ust_mesh, "DENSITY",  field%etatprim%tabscal(1))
  call output_vtk_scal(uf, ust_mesh, "PRESSURE", field%etatprim%tabscal(2))
  call output_vtk_vect(uf, ust_mesh, "VELOCITY", field%etatprim%tabvect(1))

case(solKDIF)
  write(uf,'(A,I9)') 'CELL_DATA ', ncellint
  call output_vtk_scal(uf, ust_mesh, "TEMPERATURE",  field%etatprim%tabscal(1))

case(solVORTEX)
case default
  call erreur("Developpement","solveur inconnu (output_vtk_cell)")
endselect

endsubroutine output_vtk_cell

!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! juin 2004 : ecriture de format CELL_DATA
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
!------------------------------------------------------------------------------!
