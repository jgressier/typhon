!------------------------------------------------------------------------------!
! Procedure : verify_cellvtex             Auteur : J. Gressier
!                                         Date   : Aout 2003
! Fonction                                Modif  : (cf Historique)
!   Vérification de l'ordre des sommets dans la définition de la connectivité
!   cell->vertex, en fonction des coordonnées des points
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine verify_cellvtex(mesh, cellvtex)

use TYPHMAKE
use USTMESH

implicit none

! -- Declaration des entrées --
type(st_mesh) :: mesh              ! données géométriques du maillage

! -- Declaration des entrées/sorties --
type(st_cellvtex) :: cellvtex

! -- Declaration des variables internes --
type(v3d) :: v12, v14, v34, v32
integer   :: ic, vtex
real(krp) :: test

! -- Debut de la procedure --

! --- Elements BAR ---
! Pas de correction à effectuer

! --- Elements TRI ---
! Pas de correction à effectuer

! --- Elements QUAD ---

do ic = 1, cellvtex%nquad
  v12 = mesh%vertex(cellvtex%quad%fils(ic,2),1,1) - mesh%vertex(cellvtex%quad%fils(ic,1),1,1) 
  v14 = mesh%vertex(cellvtex%quad%fils(ic,4),1,1) - mesh%vertex(cellvtex%quad%fils(ic,1),1,1) 
  v34 = mesh%vertex(cellvtex%quad%fils(ic,4),1,1) - mesh%vertex(cellvtex%quad%fils(ic,3),1,1) 
  v32 = mesh%vertex(cellvtex%quad%fils(ic,2),1,1) - mesh%vertex(cellvtex%quad%fils(ic,3),1,1)
  test = (v12.vect.v14).scal.(v34.vect.v32)
  if (test < 0_krp) then
    vtex                     = cellvtex%quad%fils(ic,4)  
    cellvtex%quad%fils(ic,4) = cellvtex%quad%fils(ic,3) 
    cellvtex%quad%fils(ic,3) = vtex
  endif
enddo

! --- Elements TETRA ---
! Pas de correction à effectuer

! --- Elements PYRA ---

! --- Elements PENTA ---

! --- Elements HEXA ---



endsubroutine verify_cellvtex

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Aout 2003 : création de la procédure (correction des QUAD)
! 
!------------------------------------------------------------------------------!
