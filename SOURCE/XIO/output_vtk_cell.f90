!-----------------------------------------------------------------------------!
! Procedure : output_vtk_cell             Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
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

! -- Declaration des entrees --
integer          :: uf            ! unite d'ecriture
type(mnu_solver) :: defsolver     ! parametres du solveur
type(st_ustmesh) :: ust_mesh      ! maillage a ecrire
type(st_field)   :: field         ! champ de valeurs

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: i
integer   :: info, ntot
type(v3d) :: vtex

! -- Debut de la procedure --


! ecriture du maillage

write(uf,'(a)')      'DATASET UNSTRUCTURED_GRID'
write(uf,'(a,i9,a)') 'POINTS ',ust_mesh%nvtex, ' float'

! coordonnees

do i = 1, ust_mesh%nvtex
  vtex = ust_mesh%mesh%vertex(i,1,1)
  write(uf,'(4e18.8)') vtex%x, vtex%y, vtex%z
enddo

! connectivite 

ust_mesh%ncell = ust_mesh%cellvtex%nbar   + &
                 ust_mesh%cellvtex%ntri   + ust_mesh%cellvtex%nquad + &
                 ust_mesh%cellvtex%ntetra + ust_mesh%cellvtex%npyra + &
                 ust_mesh%cellvtex%npenta + ust_mesh%cellvtex%nhexa

ntot =        3*ust_mesh%cellvtex%nbar
ntot = ntot + 4*ust_mesh%cellvtex%ntri
ntot = ntot + 5*ust_mesh%cellvtex%nquad
ntot = ntot + 5*ust_mesh%cellvtex%ntetra
ntot = ntot + 6*ust_mesh%cellvtex%npyra
ntot = ntot + 7*ust_mesh%cellvtex%npenta
ntot = ntot + 9*ust_mesh%cellvtex%nhexa
write(uf,'(a,2i10)') 'CELLS ',ust_mesh%ncell, ntot
print*,'nbar:',ust_mesh%cellvtex%nbar
do i = 1, ust_mesh%cellvtex%nbar
  write(uf,'(i3,2i9)') 2, ust_mesh%cellvtex%bar%fils(i,:)-1
enddo
do i = 1, ust_mesh%cellvtex%ntri
  write(uf,'(i3,3i9)') 3, ust_mesh%cellvtex%tri%fils(i,:)-1
enddo
print*,'nquad:',ust_mesh%cellvtex%nquad
do i = 1, ust_mesh%cellvtex%nquad
  write(uf,'(i3,4i9)') 4, ust_mesh%cellvtex%quad%fils(i,:)-1
enddo
do i = 1, ust_mesh%cellvtex%ntetra
  write(uf,'(i3,4i9)') 4, ust_mesh%cellvtex%tetra%fils(i,:)-1
enddo
do i = 1, ust_mesh%cellvtex%npyra
  write(uf,'(i3,5i9)') 5, ust_mesh%cellvtex%pyra%fils(i,:)-1
enddo
do i = 1, ust_mesh%cellvtex%npenta
  write(uf,'(i3,6i9)') 6, ust_mesh%cellvtex%penta%fils(i,:)-1
enddo
do i = 1, ust_mesh%cellvtex%nhexa
  write(uf,'(i3,8i9)') 8, ust_mesh%cellvtex%hexa%fils(i,:)-1
enddo

! type de cellules

write(uf,'(a,i9)') 'CELL_TYPES ',ust_mesh%ncell
do i = 1, ust_mesh%cellvtex%nbar
  write(uf,'(i2)') 3     ! VTK_LINE
enddo
do i = 1, ust_mesh%cellvtex%ntri
  write(uf,'(i2)') 5     ! VTK_TRIANGLE
enddo
do i = 1, ust_mesh%cellvtex%nquad
  write(uf,'(i2)') 9     ! VTK_QUAD
enddo
do i = 1, ust_mesh%cellvtex%ntetra
  write(uf,'(i2)') 10    ! VTK_TETRA
enddo
do i = 1, ust_mesh%cellvtex%npyra
  write(uf,'(i2)') 14    ! VTK_PYRAMID
enddo
do i = 1, ust_mesh%cellvtex%npenta
  write(uf,'(i2)') 13    ! VTK_WEDGE
enddo
do i = 1, ust_mesh%cellvtex%nhexa
  write(uf,'(i2)') 12    ! VTK_HEXAHEDRON
enddo

! -- donnees --

call calc_varprim(defsolver, field)

select case(defsolver%typ_solver)
case(solNS)
  write(uf,'(a,i9)') 'CELL_DATA ',ust_mesh%ncell
  call output_vtk_scal(uf, ust_mesh, "DENSITY",  field%etatprim%tabscal(1))
  call output_vtk_scal(uf, ust_mesh, "PRESSURE", field%etatprim%tabscal(2))
  call output_vtk_vect(uf, ust_mesh, "VELOCITY", field%etatprim%tabvect(1))

case(solKDIF)
  write(uf,'(a,i9)') 'CELL_DATA ',ust_mesh%ncell
  call output_vtk_scal(uf, ust_mesh, "TEMPERATURE",  field%etatprim%tabscal(1))

case(solVORTEX)
case default
  call erreur("Developpement","solveur inconnu (output_vtk_cell)")
endselect


endsubroutine output_vtk_cell

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : creation de la procedure
! juin 2004 : ecriture de format CELL_DATA
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
!------------------------------------------------------------------------------!
