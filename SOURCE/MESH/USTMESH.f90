!------------------------------------------------------------------------------!
! MODULE : USTMESH                        Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion de maillages
!   non structurés
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module USTMESH

use TYPHMAKE   ! Definition de la precision
use GEO3D 
use MESHBASE  ! Librairie pour les éléments géométriques de base

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
! Définition de la structure ST_USTCONNECT : Définition de la connectivité
!   Sommets, faces, cellules
!------------------------------------------------------------------------------!
!!!!!!!! A TERME, CETTE STRUCTURE DOIT ETRE SUPPRIMEE ET REMPLACEE !!!!!!!!!!!!!
!!!!!!!! PAR LES STRUCTURES DE CONNECTIVITY (MODCOM)               !!!!!!!!!!!!!
type st_ustconnect
  integer                 :: nbnodes     ! nombre de d'ensemble connectivités
  integer                 :: nbfils      ! nombre de connectivités par ensemble
  integer, dimension(:,:), pointer &
                          :: fils        ! définition de la connectivité
endtype st_ustconnect


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
!! type(st_ustconnect), pointer &           ! tableau par type d'élements (nbfils)
  type(st_ustconnect)   :: facevtex, &     ! connectivité face   -> sommets   par type
      !! non utilisé       cellface, &     ! connectivité cellule-> faces     par type
      !! non utilisé       cellvtex, &     ! connectivité cellule-> vtex      par type
                           facecell        ! connectivité face   -> cellules  par type
  integer               :: nboco          ! nombre de conditions aux limites
  type(st_ustboco), dimension(:), pointer &
                        :: boco           ! liste des conditions aux limites
endtype st_ustmesh


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustmesh, new_ustconnect
endinterface

interface delete
  module procedure delete_ustmesh, delete_ustconnect
endinterface

interface copy
  module procedure copy_ustconnect
endinterface

interface realloc
  module procedure realloc_ustconnect
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
! Procédure : allocation d'une structure USTCONNECT
!------------------------------------------------------------------------------!
subroutine new_ustconnect(conn, nbnodes, nbfils)
implicit none
type(st_ustconnect) :: conn
integer             :: nbnodes, nbfils

  conn%nbnodes = nbnodes
  conn%nbfils  = nbfils
  allocate(conn%fils(nbnodes, nbfils))

endsubroutine new_ustconnect


!------------------------------------------------------------------------------!
! Procédure : reallocation d'une structure USTCONNECT
!------------------------------------------------------------------------------!
subroutine realloc_ustconnect(conn, nbnodes, nbfils)
implicit none
type(st_ustconnect) :: conn, prov
integer             :: nbnodes,      nbfils      ! nouvelle taille
integer             :: old_nbnodes, old_nbfils   ! ancienne taille
integer             :: min_nbnodes, min_nbfils   ! ancienne taille

  prov = copy(conn)
  conn%nbnodes = nbnodes                   ! affectation des nouvelles tailles
  conn%nbfils  = nbfils 
  deallocate(conn%fils)                    ! désallocation de l'ancien tableau     
  allocate(conn%fils(nbnodes, nbfils))     ! allocation du nouveau tableau
  conn%fils(1:nbnodes, 1:nbfils) = 0       ! initialisation
  min_nbnodes = min(nbnodes, prov%nbnodes)
  min_nbfils  = min(nbfils,  prov%nbfils)  ! copie des connectivités
  conn%fils(1:min_nbnodes, 1:min_nbfils) = prov%fils(1:min_nbnodes, 1:min_nbfils) 

endsubroutine realloc_ustconnect


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure USTCONNECT par copie
!------------------------------------------------------------------------------!
function copy_ustconnect(source)
implicit none
type(st_ustconnect) :: copy_ustconnect, source

  copy_ustconnect%nbnodes = source%nbnodes
  copy_ustconnect%nbfils  = source%nbfils
  allocate(copy_ustconnect%fils(copy_ustconnect%nbnodes, copy_ustconnect%nbfils))
  copy_ustconnect%fils    = source%fils

endfunction copy_ustconnect


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure USTCONNECT
!------------------------------------------------------------------------------!
subroutine delete_ustconnect(conn)
implicit none
type(st_ustconnect) :: conn

  if (associated(conn%fils)) deallocate(conn%fils)

endsubroutine delete_ustconnect




endmodule USTMESH

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct 2002 (v0.0.1b): création du module
!------------------------------------------------------------------------------!



