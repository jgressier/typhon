!------------------------------------------------------------------------------!
! MODULE : USTMESH                        Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion de maillages
!   non structures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module USTMESH

use TYPHMAKE      ! machine accuracy
use GEO3D 
use MESHBASE      ! geometrical basic elements
use CONNECTIVITY  ! lists & connectivity 
use DEFFIELD

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure ST_CELLVTEX : Definition de connectivite CELL -> VERTEX
!   une connectivite speciale est definie pour une meilleure gestions des
!   actions selon le type des elements.
!------------------------------------------------------------------------------!
type st_cellvtex
  integer          :: dim                      ! dimension spatiale des elements (2D/3D)
  integer          :: nbar, ntri, nquad, &     ! nombre d'elements par famille
                      ntetra, npyra, npenta, nhexa  
  type(st_connect) :: bar, tri, quad,    &     ! definition des elements
                      tetra, pyra, penta, hexa
  integer, dimension(:), pointer &
                   :: ibar, itri, iquad, &     ! redirection d'index vers "icell" de ustmesh
                      itetra, ipyra, ipenta, ihexa 
endtype st_cellvtex


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTBOCO : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_ustboco
  character(len=strlen)          :: family     ! nom de famille
  integer                        :: idefboco   ! pointeur index vers la definition 
                                               ! des conditions aux limites dans defsolver
  integer                        :: nface      ! nombre de faces concernees
  integer, dimension(:), pointer :: iface      ! liste des faces concernees par
                                               ! les conditions aux limites
  type(st_genericfield), pointer &
                         :: bocofield          ! liste chainee de champs generiques

  !! type(st_solvboco), pointer    :: boco      ! condition aux limites associee
  !! type(st_strboco),  pointer :: next       ! (liste) condition suivante
endtype st_ustboco


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTMESH : Maillage non structure
!------------------------------------------------------------------------------!
! les tableaux de faces et de cellules contiennent les elements internes puis
! les elements limites.

type st_ustmesh
  integer               :: id              ! numero de domaine
  !integer              :: level           ! niveau multigrille
  !integer               :: nbdim           ! nombre de dimension du maillage
  integer               :: nvtex, nface, ncell   ! nombre de sommets, faces et cellules
  integer               :: nface_int, ncell_int  ! nombre de faces et cellules internes
  integer               :: nface_lim, ncell_lim  ! nombre de faces et cellules limites
  type(st_mesh)         :: mesh            ! maillage associe (geometrie)
  type(st_connect)      :: facevtex, &     ! connectivite face   -> sommets   par type
                           facecell        ! connectivite face   -> cellules  par type
                                           ! SUPPOSED TO INDEX LOWER INDEX CELL BEFORE
  type(st_cellvtex)     :: cellvtex        ! connectivite cellule-> vtex      par type
  integer               :: nboco          ! nombre de conditions aux limites
  type(st_ustboco), dimension(:), pointer &
                        :: boco           ! liste des conditions aux limites
endtype st_ustmesh



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustmesh, new_cellvtex, new_ustboco
endinterface

interface init
  module procedure init_cellvtex
endinterface

interface delete
  module procedure delete_ustmesh, delete_cellvtex, delete_ustboco
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine new_ustmesh(mesh, ncell, nface, nvtex)
implicit none
type(st_ustmesh) :: mesh
integer       :: ncell, nface, nvtex

  print*,"!!! pas d'allocation dans new_ustmesh !!!"
  stop
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
! Procedure : desallocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine delete_ustmesh(mesh)
implicit none
type(st_ustmesh) :: mesh
integer          :: i

  call delete(mesh%mesh)
  call delete(mesh%facevtex)
  call delete(mesh%facecell)
  call delete(mesh%cellvtex)
  do i = 1, mesh%nboco 
    call delete(mesh%boco(i))
  enddo
  deallocate(mesh%boco)
  !deallocate(mesh%center, mesh%vertex, mesh%volume)
  !deallocate(mesh%iface, mesh%jface)
  !if (mesh%kdim /= 1) deallocate(mesh%kface)

endsubroutine delete_ustmesh


!------------------------------------------------------------------------------!
! Procedure : creation d'une structure BOCO dans USTMESH
!------------------------------------------------------------------------------!
subroutine createboco(mesh, nboco)
implicit none
type(st_ustmesh) :: mesh
integer          :: nboco

  mesh%nboco = nboco
  allocate(mesh%boco(nboco))

endsubroutine createboco


!------------------------------------------------------------------------------!
! Procedure : recherche d'une condition limite dans USTMESH
!------------------------------------------------------------------------------!
function pboco_ustmesh(umesh, name) result(pboco)
implicit none
type(st_ustboco), pointer :: pboco
type(st_ustmesh)          :: umesh
character(len=strlen)     :: name
! -- variables internes --
integer :: i, info

  info = 0
  do i = 1, umesh%nboco
    if (samestring(umesh%boco(i)%family, name)) then
      info  =  info + 1
      pboco => umesh%boco(i)
    endif
  enddo
  if (info /= 1) call erreur("structure","plusieurs noms de conditions limites identiques")
  
endfunction pboco_ustmesh


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine new_ustboco(bc, nom, n)
implicit none
type(st_ustboco)      :: bc
character(len=strlen) :: nom
integer               :: n

  bc%family = nom
  bc%nface  = n
  allocate(bc%iface(n))

endsubroutine new_ustboco


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine delete_ustboco(bc)
implicit none
type(st_ustboco) :: bc
integer          :: i

  deallocate(bc%iface)

endsubroutine delete_ustboco


!------------------------------------------------------------------------------!
! Procedure : allocation des tableaux d'une structure CELLVTEX
!------------------------------------------------------------------------------!
subroutine new_cellvtex(conn)
implicit none
type(st_cellvtex) :: conn

  if (conn%nbar   /= 0) then
    call new(conn%bar, conn%nbar, 2)
    allocate(conn%ibar(conn%nbar))
  endif
  if (conn%ntri   /= 0) then
    call new(conn%tri, conn%ntri, 3)
    allocate(conn%itri(conn%ntri))
  endif
  if (conn%nquad  /= 0) then
    call new(conn%quad, conn%nquad, 4)
    allocate(conn%iquad(conn%nquad))
  endif
  if (conn%ntetra /= 0) then
    call new(conn%tetra, conn%ntetra, 4)
    allocate(conn%itetra(conn%ntetra))
  endif
  if (conn%npyra  /= 0) then
    call new(conn%pyra, conn%npyra, 5)
    allocate(conn%ipyra(conn%npyra))
  endif
  if (conn%npenta /= 0) then
    call new(conn%penta, conn%npenta, 6)
    allocate(conn%ipenta(conn%npenta))
  endif
  if (conn%nhexa  /= 0) then
    call new(conn%hexa, conn%nhexa, 8)
    allocate(conn%ihexa(conn%nhexa))
  endif

endsubroutine new_cellvtex


!------------------------------------------------------------------------------!
! Procedure : Intialisation des tableaux d'une structure CELLVTEX
!------------------------------------------------------------------------------!
subroutine init_cellvtex(conn)
implicit none
type(st_cellvtex) :: conn

  conn%nbar   = 0
  conn%ntri   = 0
  conn%nquad  = 0
  conn%ntetra = 0
  conn%npyra  = 0
  conn%npenta = 0
  conn%nhexa  = 0

endsubroutine init_cellvtex


!------------------------------------------------------------------------------!
! Procedure :desallocation d'une structure CELLVTEX
!------------------------------------------------------------------------------!
subroutine delete_cellvtex(conn)
implicit none
type(st_cellvtex) :: conn

  if (conn%nbar   /= 0) then
    call delete(conn%bar)
    deallocate(conn%ibar)
  endif
  if (conn%ntri   /= 0) then
    call delete(conn%tri)
    deallocate(conn%itri)
  endif
  if (conn%nquad  /= 0) then
    call delete(conn%quad)
    deallocate(conn%iquad)
  endif
  if (conn%ntetra /= 0) then
    call delete(conn%tetra)
    deallocate(conn%itetra)
  endif
  if (conn%npyra  /= 0) then
    call delete(conn%pyra)
    deallocate(conn%ipyra)
  endif
  if (conn%npenta /= 0) then
    call delete(conn%penta)
    deallocate(conn%ipenta)
  endif
  if (conn%nhexa  /= 0) then
    call delete(conn%hexa)
    deallocate(conn%ihexa)
  endif

endsubroutine delete_cellvtex


!------------------------------------------------------------------------------!
! Fonction : face_invtexlist
! Teste la face est incluse (selon ses sommets) dans une liste de sommets
!------------------------------------------------------------------------------!
logical function face_invtexlist(nsf, face, nsl, vtexlist)
implicit none
! -- Entrees --
integer                   :: nsf, nsl         ! nombre de sommets de la face et de la liste
integer, dimension(1:nsf) :: face             ! face a rechercher
integer, dimension(1:nsl) :: vtexlist         ! liste des sommets
! -- Variables internes --
integer :: isf, isl
logical :: same_som

  ! -- Debut de procedure
   
  do isf = 1, nsf   ! boucle sur les sommets de la face
    ! recherche sommet par sommet de FACE dans VTEXLIST

    do isl = 1, nsl
      same_som = (face(isf)==vtexlist(isl)).or.(face(isf)==0)   ! la face peut etre definie avec des 0
      if (same_som) exit    ! le sommet a ete trouve : on passe au suivant (de la face)
    enddo

    if (.not.same_som) exit   ! un sommet non trouve de la face suffit a quitter
  enddo

  face_invtexlist = same_som

endfunction face_invtexlist


!------------------------------------------------------------------------------!
! Fonction : typgeo : type de geometrie du maillage
!------------------------------------------------------------------------------!
character function typgeo(umesh)
implicit none
type(st_ustmesh) :: umesh

  typgeo = umesh%mesh%info%geom

endfunction typgeo


endmodule USTMESH

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 : creation du module
! juil 2003 : suppression des structures USTCONNECT, definition dans CONNECTIVITY
!             creation d'une structure de connectivite CELLVTEX
!------------------------------------------------------------------------------!



