!------------------------------------------------------------------------------!
! MODULE : USTMESH                        Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion de maillages
!   non structurés
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module USTMESH

use TYPHMAKE      ! Definition de la precision
use GEO3D 
use MESHBASE      ! Librairie pour les éléments géométriques de base
use CONNECTIVITY  ! Librairie de gestion de listes et connectivités

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Définition de la structure ST_USTBOUND : Définition des conditions aux limites
!------------------------------------------------------------------------------!
type st_ustboco
  character(len=strlen)          :: family     ! nom de famille
  integer                        :: idefboco   ! pointeur index vers la définition 
                                               ! des conditions aux limites dans defsolver
  integer                        :: nface      ! nombre de faces concernées
  integer, dimension(:), pointer :: iface      ! liste des faces concernées par
                                               ! les conditions aux limites

  !! type(st_solvboco), pointer    :: boco      ! condition aux limites associée
  !! type(st_strboco),  pointer :: next       ! (liste) condition suivante
endtype st_ustboco


!------------------------------------------------------------------------------!
! Définition de la structure ST_USTMESH : Maillage non structuré
!------------------------------------------------------------------------------!
! les tableaux de faces et de cellules contiennent les éléments internes puis
! les éléments limites.

type st_ustmesh
  integer               :: id              ! numero de domaine
  !integer              :: level           ! niveau multigrille
  integer               :: nbdim           ! nombre de dimension du maillage
  integer               :: nvtex, nface, ncell   ! nombre de sommets, faces et cellules
  integer               :: nface_int, ncell_int  ! nombre de faces et cellules internes
  integer               :: nface_lim, ncell_lim  ! nombre de faces et cellules limites
  type(st_mesh)         :: mesh            ! maillage associé (géométrie)
!! type(st_connect), pointer &           ! tableau par type d'élements (nbfils)
  type(st_connect)      :: facevtex, &     ! connectivité face   -> sommets   par type
      !! non utilisé       cellface, &     ! connectivité cellule-> faces     par type
      !! non utilisé       cellvtex, &     ! connectivité cellule-> vtex      par type
                           facecell        ! connectivité face   -> cellules  par type
  integer               :: nboco          ! nombre de conditions aux limites
  type(st_ustboco), dimension(:), pointer &
                        :: boco           ! liste des conditions aux limites
endtype st_ustmesh


!------------------------------------------------------------------------------!
! structure ST_CELLVTEX : Définition de connectivité CELL -> VERTEX
!   une connectivité spéciale est définie pour une meilleure gestions des
!   actions selon le type des éléments.
!------------------------------------------------------------------------------!
type st_cellvtex
  integer          :: dim                      ! dimension spatiale des éléments (2D/3D)
  integer          :: nbar, ntri, nquad, &     ! nombre d'éléments par famille
                      ntetra, npyra, npenta, nhexa  
  type(st_connect) :: bar, tri, quad,    &     ! définition des éléments
                      tetra, pyra, penta, hexa  
endtype st_cellvtex



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustmesh
endinterface

interface delete
  module procedure delete_ustmesh, delete_cellvtex
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine new_ustmesh(mesh, ncell, nface, nvtex)
implicit none
type(st_ustmesh) :: mesh
integer       :: ncell, nface, nvtex

  !mesh%idim = idim
  !mesh%jdim = jdim
  !mesh%kdim = kdim
  !if (kdim /= 1) then ! CAS 3D
  !  allocate(mesh%center(0:idim+1, 0:jdim+1, 0:kdim+1))
  !  allocate(mesh%vertex(1:idim+1, 1:jdim+1, 1:kdim+1))
  !  allocate(mesh% iface(1:idim+1, 1:jdim,   1:kdim))
  !  allocate(mesh% jface(1:idim,   1:jdim+1, 1:kdim))
  !  allocate(mesh% kface(1:idim,   1:jdim,   1:kdim+1))
  !  allocate(mesh%volume(1:idim,   1:jdim,   1:kdim))
  !else                ! CAS 2D
  !  allocate(mesh%center(0:idim+1, 0:jdim+1, 1))
  !  allocate(mesh%vertex(1:idim+1, 1:jdim+1, 1))
  !  allocate(mesh% iface(1:idim+1, 1:jdim,   1))
  !  allocate(mesh% jface(1:idim,   1:jdim+1, 1))
  !  nullify(mesh%kface)
  !  allocate(mesh%volume(1:idim,   1:jdim,   1))
  !endif
  !nullify(mesh%facevtex)
  !nullify(mesh%cellvtex)
  !nullify(mesh%facecell)
  !nullify(mesh%cellface)

endsubroutine new_ustmesh


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine delete_ustmesh(mesh)
implicit none
type(st_ustmesh) :: mesh

  call delete(mesh%mesh)
  !deallocate(mesh%center, mesh%vertex, mesh%volume)
  !deallocate(mesh%iface, mesh%jface)
  !if (mesh%kdim /= 1) deallocate(mesh%kface)

endsubroutine delete_ustmesh


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure CELLVTEX
!------------------------------------------------------------------------------!
subroutine delete_cellvtex(conn)
implicit none
type(st_cellvtex) :: conn

  if (conn%nbar   /= 0) call delete(conn%bar)
  if (conn%ntri   /= 0) call delete(conn%tri)
  if (conn%nquad  /= 0) call delete(conn%quad)
  if (conn%ntetra /= 0) call delete(conn%tetra)
  if (conn%npyra  /= 0) call delete(conn%pyra)
  if (conn%npenta /= 0) call delete(conn%penta)
  if (conn%nhexa  /= 0) call delete(conn%hexa)

endsubroutine delete_cellvtex




endmodule USTMESH

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 : création du module
! juil 2003 : suppression des structures USTCONNECT, définition dans CONNECTIVITY
!             création d'une structure de connectivité CELLVTEX
!------------------------------------------------------------------------------!



