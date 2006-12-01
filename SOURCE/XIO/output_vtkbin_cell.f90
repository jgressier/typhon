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
integer   :: info, ntot
type(v3d) :: vtex
type(st_cellvtex) :: cvtx

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

write(str_w,'(A,2I10)') 'CELLS ', ncellint, ntot
call writestr(uf, trim(str_w))
do i = 1, cvtx%nbar
  write(uf) 2, cvtx%bar%fils(i,:)-1
enddo
do i = 1, cvtx%ntri
  write(uf) 3, cvtx%tri%fils(i,:)-1
enddo
do i = 1, cvtx%nquad
  write(uf) 4, cvtx%quad%fils(i,:)-1
enddo
do i = 1, cvtx%ntetra
  write(uf) 4, cvtx%tetra%fils(i,:)-1
enddo
do i = 1, cvtx%npyra
  write(uf) 5, cvtx%pyra%fils(i,:)-1
enddo
do i = 1, cvtx%npenta
  write(uf) 6, cvtx%penta%fils(i,:)-1
enddo
do i = 1, cvtx%nhexa
  write(uf) 8, cvtx%hexa%fils(i,:)-1
enddo
call writereturn(uf)

! type de cellules

write(str_w,'(A,I9,A)') 'CELL_TYPES ', ncellint, ' int'
call writestr(uf, trim(str_w))

do i = 1, cvtx%nbar
  write(uf)  3    ! VTK_LINE
enddo
do i = 1, cvtx%ntri
  write(uf)  5    ! VTK_TRIANGLE
enddo
do i = 1, cvtx%nquad
  write(uf)  9    ! VTK_QUAD
enddo
do i = 1, cvtx%ntetra
  write(uf) 10    ! VTK_TETRA
enddo
do i = 1, cvtx%npyra
  write(uf) 14    ! VTK_PYRAMID
enddo
do i = 1, cvtx%npenta
  write(uf) 13    ! VTK_WEDGE
enddo
do i = 1, cvtx%nhexa
  write(uf) 12    ! VTK_HEXAHEDRON
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
