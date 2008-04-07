!-----------------------------------------------------------------------------!
! Procedure : output_vtkbin_cell                Auteur : J. Gressier
!                                               Date   : April 2006
! Function
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine output_vtkbin_cell(uf, defsolver, ust_mesh, field)

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
integer   :: info, ntot, ielem, vtktype, nvtex
type(v3d) :: vtex

! -- BODY --


! ecriture du maillage

call writestr(uf,'DATASET UNSTRUCTURED_GRID')
write(str_w,'(A,I9,A)') 'POINTS ', ust_mesh%nvtex, ' double'
call writestr(uf, trim(str_w))

! coordonnees

do i = 1, ust_mesh%nvtex
  vtex = ust_mesh%mesh%vertex(i,1,1)
  write(uf) real(vtex%x, kind=8), real(vtex%y, kind=8), real(vtex%z, kind=8)
enddo
call writereturn(uf)

! CELLVTEX connectivity

ncellint       = ust_mesh%ncell_int
ntot           = 0                       ! number of written integer in CELLVTEX connectivity

do ielem = 1, ust_mesh%cellvtex%ntype
  nvtex = ust_mesh%cellvtex%elem(ielem)%nvtex
  ntot  = ntot + (nvtex+1)*ust_mesh%cellvtex%elem(ielem)%nelem
enddo

write(str_w,'(A,2I10)') 'CELLS ', ncellint, ntot
call writestr(uf, trim(str_w))

do ielem = 1, ust_mesh%cellvtex%ntype

  nvtex = ust_mesh%cellvtex%elem(ielem)%nvtex
  do i = 1, ust_mesh%cellvtex%elem(ielem)%nelem
    write(uf) nvtex, ust_mesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
  enddo

enddo

! -- CELL TYPES --

write(str_w,'(A,I9,A)') 'CELL_TYPES ', ncellint, ' int'
call writestr(uf, trim(str_w))

do ielem = 1, ust_mesh%cellvtex%ntype

  select case(ust_mesh%cellvtex%elem(ielem)%elemtype)
  case(elem_bar2)
    vtktype = 3    ! VTK_LINE 
  case(elem_tri3)
    vtktype = 5    ! VTK_TRIANGLE
  case(elem_quad4)
    vtktype = 9    ! VTK_QUAD
  case(elem_ngon)
    vtktype = 7    ! VTK_POLYGON
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

  do i = 1, ust_mesh%cellvtex%elem(ielem)%nelem
    write(uf) vtktype
  enddo

enddo

call writereturn(uf)

! -- donnees --

call calc_varprim(defsolver, field)

select case(defsolver%typ_solver)
case(solNS)
  write(str_w,'(A,I9)') 'CELL_DATA ', ncellint
  call writestr(uf, trim(str_w))
  call output_vtkbin_scal(uf, ust_mesh, "DENSITY",  field%etatprim%tabscal(1))
  call output_vtkbin_scal(uf, ust_mesh, "PRESSURE", field%etatprim%tabscal(2))
  call output_vtkbin_vect(uf, ust_mesh, "VELOCITY", field%etatprim%tabvect(1))

case(solKDIF)
  write(str_w,'(A,I9)') 'CELL_DATA ', ncellint
  call writestr(uf, trim(str_w))
  call output_vtkbin_scal(uf, ust_mesh, "TEMPERATURE",  field%etatprim%tabscal(1))

case(solVORTEX)
case default
  call erreur("Developpement","solveur inconnu (output_vtkbin_cell)")
endselect

endsubroutine output_vtkbin_cell

!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! juin 2004 : ecriture de format CELL_DATA
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
!------------------------------------------------------------------------------!
