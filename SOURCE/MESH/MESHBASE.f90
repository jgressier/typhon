!------------------------------------------------------------------------------!
! MODULE : MESHBASE                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion des elements
!   geometriques de base (face...)
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module MESHBASE

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! elements geometriques

implicit none

! -- Variables globales du module -------------------------------------------

! type de maillage
character, parameter :: msh_1Dcurv = '1'
character, parameter :: msh_2Dplan = '2'
character, parameter :: msh_2Dcurv = 'C'
character, parameter :: msh_3D     = '3'

! -- Definition des caracteres caracteristiques pour le type de maillage --
!character, parameter :: mshSTR = 'S'   (defini dans VARCOM)
!character, parameter :: mshUST = 'U'

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure INFO_MESH
!------------------------------------------------------------------------------!
type info_mesh
  character :: geom                ! type de maillage (cf constantes)
  type(v3d) :: min, max            ! coordonnees min et max des vertex
  real(krp) :: minscale, maxscale  ! echelle de longueur (min et max)
endtype


!------------------------------------------------------------------------------!
! Definition de la structure ST_FACE : face de cellule
!------------------------------------------------------------------------------!
type st_face
  type(v3d)   :: normale        ! normale a la face, orientee indice croissant
  type(v3d)   :: centre         ! centre de face
  real(krp)   :: surface        ! valeur de la surface de la face
endtype st_face


!------------------------------------------------------------------------------!
! Definition de la structure ST_MESH : liste de vertex, faces, centres, volumes
!------------------------------------------------------------------------------!
type st_mesh
  type(info_mesh) :: info
  integer         :: idim, jdim, kdim      ! indices max des cellules 
  integer         :: nvtex                 ! nombre de sommets
  integer         :: nface
  integer         :: ncell                 ! nombre de faces et cellules totales
  type(v3d), dimension(:,:,:), pointer &  ! coordonnees des sommets et centres
                  :: vertex, centre        ! de cellules (i,j,k)
  type(st_face), dimension(:,:,:), pointer &
                  :: iface !, jface, kface   ! tableaux de faces
  real(krp), dimension(:,:,:), pointer &
                  :: volume                ! volume des cellules
endtype st_mesh


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_mesh
endinterface

interface delete
  module procedure delete_mesh
endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine new_mesh(mesh, ncell, nface, nvtex)
implicit none
type(st_mesh) :: mesh
integer       :: ncell, nface, nvtex

  mesh%ncell = ncell
  mesh%nface = nface
  mesh%nvtex = nvtex
  if (ncell /= 0) then
    allocate(mesh%centre(1:ncell, 1,1))
    allocate(mesh%volume(1:ncell, 1,1))
  endif
  if (nface /= 0) allocate(mesh% iface(1:nface, 1,1))
  if (nvtex /= 0) allocate(mesh%vertex(1:nvtex, 1,1))

endsubroutine new_mesh


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine delete_mesh(mesh)
implicit none
type(st_mesh) :: mesh

  if (mesh%ncell /= 0) deallocate(mesh%centre, mesh%volume)
  if (mesh%nface /= 0) deallocate(mesh%iface)
  if (mesh%nvtex /= 0) deallocate(mesh%vertex)
  
endsubroutine delete_mesh



endmodule MESHBASE

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 : creation du module
! fev  2004 : suppression de certains elements propres au structure
!             structure information de MESH
!             redefintion de new_mesh (allocation de non structure)
!------------------------------------------------------------------------------!
