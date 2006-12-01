!-----------------------------------------------------------------------------!
! Procedure : output_vtk_cell                   Auteur : J. Gressier
!                                               Date   : Avril 2004
! Function                                      Modif  : (cf historique)
!   Ecriture fichier des champs NON STRUCTURES de chaque zone au format VTK
!   Valeurs au centre des cellules
!
! Defauts/Limitations/Divers :
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
type(st_cellvtex) :: cvtx

! -- BODY --


! ecriture du maillage

write(uf,'(A)')      'DATASET UNSTRUCTURED_GRID'
write(uf,'(A,I9,A)') 'POINTS ', ust_mesh%nvtex, ' double'

! coordonnees

do i = 1, ust_mesh%nvtex
  vtex = ust_mesh%mesh%vertex(i,1,1)
  write(uf,'(1P,4E17.8E3)') vtex%x, vtex%y, vtex%z
enddo

! variable interne

cvtx = ust_mesh%cellvtex

! connectivite 

ncellint       = cvtx%nbar   + &
                 cvtx%ntri   + &
                 cvtx%nquad  + &
                 cvtx%ntetra + &
                 cvtx%npyra  + &
                 cvtx%npenta + &
                 cvtx%nhexa

ntot =        3*cvtx%nbar
ntot = ntot + 4*cvtx%ntri
ntot = ntot + 5*cvtx%nquad
ntot = ntot + 5*cvtx%ntetra
ntot = ntot + 6*cvtx%npyra
ntot = ntot + 7*cvtx%npenta
ntot = ntot + 9*cvtx%nhexa

write(uf,'(A,2i10)') 'CELLS ', ncellint, ntot

do i = 1, cvtx%nbar
  write(uf,'(i3,2I9)') 2, cvtx%bar%fils(i,:)-1
enddo
do i = 1, cvtx%ntri
  write(uf,'(i3,3I9)') 3, cvtx%tri%fils(i,:)-1
enddo
do i = 1, cvtx%nquad
  write(uf,'(i3,4I9)') 4, cvtx%quad%fils(i,:)-1
enddo
do i = 1, cvtx%ntetra
  write(uf,'(i3,4I9)') 4, cvtx%tetra%fils(i,:)-1
enddo
do i = 1, cvtx%npyra
  write(uf,'(i3,5I9)') 5, cvtx%pyra%fils(i,:)-1
enddo
do i = 1, cvtx%npenta
  write(uf,'(i3,6I9)') 6, cvtx%penta%fils(i,:)-1
enddo
do i = 1, cvtx%nhexa
  write(uf,'(i3,8I9)') 8, cvtx%hexa%fils(i,:)-1
enddo

! type de cellules

write(uf,'(A,I9)') 'CELL_TYPES ', ncellint

do i = 1, cvtx%nbar
  write(uf,'(I2)')  3    ! VTK_LINE
enddo
do i = 1, cvtx%ntri
  write(uf,'(I2)')  5    ! VTK_TRIANGLE
enddo
do i = 1, cvtx%nquad
  write(uf,'(I2)')  9    ! VTK_QUAD
enddo
do i = 1, cvtx%ntetra
  write(uf,'(I2)') 10    ! VTK_TETRA
enddo
do i = 1, cvtx%npyra
  write(uf,'(I2)') 14    ! VTK_PYRAMID
enddo
do i = 1, cvtx%npenta
  write(uf,'(I2)') 13    ! VTK_WEDGE
enddo
do i = 1, cvtx%nhexa
  write(uf,'(I2)') 12    ! VTK_HEXAHEDRON
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
