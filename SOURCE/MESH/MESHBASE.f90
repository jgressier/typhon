!------------------------------------------------------------------------------!
! MODULE : MESHBASE                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : 
!   Bibliotheque de procedures et fonctions pour la gestion des éléments
!   géométriques de base (face...)
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module MESHBASE

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! éléments géométriques

implicit none

! -- Variables globales du module -------------------------------------------

! -- Définition des caractères caractéristiques pour le type de maillage --
!character, parameter :: mshSTR = 'S'   (défini dans VARCOM)
!character, parameter :: mshUST = 'U'

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Définition de la structure ST_FACE : face de cellule
!------------------------------------------------------------------------------!
type st_face
  type(v3d)   :: normale        ! normale à la face, orientée indice croissant
  type(v3d)   :: centre         ! centre de face
  real(krp)   :: surface        ! valeur de la surface de la face
endtype st_face


!------------------------------------------------------------------------------!
! Définition de la structure ST_MESH : bloc de cellules structurées ou non
!------------------------------------------------------------------------------!
! POUR UN MAILLAGE STRUCTURE :
!y
!                         ^ jface(i,j+1)
!                         |                  
!     vertex(i,j+1)       |         vertex(i+1,j+1)  
!                 o----------------o    
!                 |                |    
!                 |                |    
!                 |       O        |---> iface(i+1,j)
!                 |   centre(i,j)  |     
!                 |                |             
!                 o----------------o             
!       vertex(i,j)                 vertex(i+1,j) 
!
! Soit un maillage de ti*tj sommets. On a (ti-1)*(tj-1) cellules
! idim = ti - 1   et respectivement pour j et k
! dimensions de    centre(0:idim+1, 1:jdim+1, 1:kdim+1)
!                  vertex(1:idim+1, 1:jdim+1, 1:kdim+1)
!                   iface(1:idim+1, 1:jdim,   1:kdim)
!                   jface(1:idim,   1:jdim+1, 1:kdim)
!                   kface(1:idim,   1:jdim,   1:kdim+1)
! Les lignes et colonnes de (centre) indicées 0 et dim+1 sont les centres
! des cellules fictives (de volume nul) donc les centres des faces 
! correspondantes
!
! POUR UN MAILLAGE NON STRUCTURE :
!
! seul le premier indice des tableaux :vertex:centre:volume:iface: varient
! les tableaux jface et kface ne sont pas alloués. On donc les dimensions
!   vertex:centre:volume (1:nvtex, 1, 1)
!   iface                (1:nvtex, 1, 1)
!
type st_mesh
  character      :: meshtyp               ! type de maillage (mshSTR ou mshUST)
  integer        :: idim, jdim, kdim      ! indices max des cellules 
  integer        :: nvtex                 ! nombre de sommets
  integer        :: nface,     ncell      ! nombre de faces et cellules totales
  type(v3d), dimension(:,:,:), pointer &  ! coordonnées des sommets et centres
                 :: vertex, centre        ! de cellules (i,j,k)
  type(st_face), dimension(:,:,:), pointer &
                 :: iface, jface, kface   ! tableaux de faces
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
! Procédure : allocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine new_mesh(mesh, idim, jdim, kdim)
implicit none
type(st_mesh) :: mesh
integer       :: idim, jdim, kdim

  print*,'!!!!!!!!! Attention : allocation spécifique en structuré !!!!!!!!!!!!'
  mesh%idim = idim
  mesh%jdim = jdim
  mesh%kdim = kdim
  if (kdim /= 1) then ! CAS 3D
    allocate(mesh%centre(0:idim+1, 0:jdim+1, 0:kdim+1))
    allocate(mesh%vertex(1:idim+1, 1:jdim+1, 1:kdim+1))
    allocate(mesh% iface(1:idim+1, 1:jdim,   1:kdim))
    allocate(mesh% jface(1:idim,   1:jdim+1, 1:kdim))
    allocate(mesh% kface(1:idim,   1:jdim,   1:kdim+1))
    allocate(mesh%volume(1:idim,   1:jdim,   1:kdim))
  else                ! CAS 2D
    allocate(mesh%centre(0:idim+1, 0:jdim+1, 1))
    allocate(mesh%vertex(1:idim+1, 1:jdim+1, 1))
    allocate(mesh% iface(1:idim+1, 1:jdim,   1))
    allocate(mesh% jface(1:idim,   1:jdim+1, 1))
    nullify(mesh%kface)
    allocate(mesh%volume(1:idim,   1:jdim,   1))
  endif

endsubroutine new_mesh


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine delete_mesh(mesh)
implicit none
type(st_mesh) :: mesh

  deallocate(mesh%centre, mesh%vertex, mesh%volume)

  deallocate(mesh%iface)

  if (mesh%jdim > 1) deallocate(mesh%jface)

  if (mesh%kdim > 1) deallocate(mesh%kface)

endsubroutine delete_mesh




endmodule MESHBASE
