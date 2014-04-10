!------------------------------------------------------------------------------!
! MODULE : CONNECTIVITY 
! 
! General connectivities
!
!------------------------------------------------------------------------------!

module CONNECTIVITY

use IOCFD

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure ST_ELEMC : Definition d'un element de connectivite 
!------------------------------------------------------------------------------!
type st_elemc
  integer              :: nelem     ! nombre de connectivites
  integer, allocatable :: elem(:)   ! definition de la connectivite
endtype st_elemc

!------------------------------------------------------------------------------!
! structure ST_CONNECT : Definition de connectivite a nombre de fils constants
!------------------------------------------------------------------------------!
type st_connect
  integer                 :: nbnodes     ! nombre de d'ensemble connectivites
  integer                 :: nbfils      ! nombre de connectivites par ensemble
  integer, dimension(:,:), pointer &
                          :: fils        ! definition de la connectivite
endtype st_connect

!------------------------------------------------------------------------------!
! structure ST_GENCONNECT : Definition de connectivite a nombre de fils variables
!------------------------------------------------------------------------------!
type st_genconnect
  integer                               :: nbnodes   ! number of nodes
  type(st_elemc), dimension(:), pointer :: node      ! node array
endtype st_genconnect


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_elemc, new_connect
endinterface

interface new_genconnect
  module procedure new_genconnect_empty, new_genconnect_nelem, new_genconnect_arraynelem
endinterface

interface delete
  module procedure delete_elemc, delete_connect, delete_genconnect
endinterface

interface copy
  module procedure copy_connect
endinterface

interface st_allocated
  module procedure allocated_elemc, allocated_connect
endinterface

interface realloc
  module procedure realloc_connect
endinterface

interface index
  module procedure index_int, elemc_index_int
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure ELEMC
!------------------------------------------------------------------------------!
subroutine new_elemc(conn, dim)
implicit none
type(st_elemc) :: conn
integer        :: dim

  conn%nelem  = dim
  allocate(conn%elem(dim))

endsubroutine new_elemc


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure ELEMC
!------------------------------------------------------------------------------!
subroutine new_elemc_copy(conn, conn2)
implicit none
type(st_elemc) :: conn, conn2
integer :: i

  conn%nelem  = conn2%nelem
  allocate(conn%elem(size(conn2%elem)))
  forall (i=1:conn%nelem) conn%elem(i) = conn2%elem(i)

endsubroutine new_elemc_copy


!------------------------------------------------------------------------------!
! Procedure : allocation test for ELEMC
!------------------------------------------------------------------------------!
logical function allocated_elemc(conn)
implicit none
type(st_elemc) :: conn

!  allocated_elemc = associated(conn%elem)
  allocated_elemc = allocated(conn%elem)

endfunction allocated_elemc


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure CONNECT
!------------------------------------------------------------------------------!
subroutine new_connect(conn, nbnodes, nbfils)
implicit none
type(st_connect) :: conn
integer          :: nbnodes, nbfils

  conn%nbnodes = nbnodes
  conn%nbfils  = nbfils
  allocate(conn%fils(nbnodes, nbfils))

endsubroutine new_connect


!------------------------------------------------------------------------------!
! Procedure : allocation test for CONNECT
!------------------------------------------------------------------------------!
logical function allocated_connect(conn)
implicit none
type(st_connect) :: conn

  allocated_connect = associated(conn%fils)
!  allocated_connect = allocated(conn%fils)

endfunction allocated_connect


!------------------------------------------------------------------------------!
! Procedure : reallocation d'une structure CONNECT
!------------------------------------------------------------------------------!
subroutine realloc_connect(conn, nbnodes, nbfils)
implicit none
type(st_connect) :: conn, prov
integer             :: i, j, nbnodes, nbfils      ! nouvelle taille
integer             :: old_nbnodes, old_nbfils   ! ancienne taille
integer             :: min_nbnodes, min_nbfils   ! ancienne taille

  prov = copy(conn)
  conn%nbnodes = nbnodes                   ! affectation des nouvelles tailles
  conn%nbfils  = nbfils 
  deallocate(conn%fils)                    ! desallocation de l'ancien tableau     
  allocate(conn%fils(nbnodes, nbfils))     ! allocation du nouveau tableau
  conn%fils(1:nbnodes, 1:nbfils) = 0       ! initialisation
  min_nbnodes = min(nbnodes, prov%nbnodes)
  min_nbfils  = min(nbfils,  prov%nbfils)  ! copie des connectivites
  forall(i=1:min_nbnodes, j=1:min_nbfils) conn%fils(i,j) = prov%fils(i,j) 
  call delete(prov)

endsubroutine realloc_connect


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure CONNECT par copie
!------------------------------------------------------------------------------!
function copy_connect(source)
implicit none
type(st_connect) :: copy_connect, source
integer          :: nnodes, nfils
integer          :: in

  nnodes = source%nbnodes
  nfils  = source%nbfils
  copy_connect%nbnodes = source%nbnodes
  copy_connect%nbfils  = source%nbfils
  allocate(copy_connect%fils(nnodes, nfils))
  do in = 1, nnodes
    copy_connect%fils(in, 1:nfils) = source%fils(in, 1:nfils)
  enddo

endfunction copy_connect


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure ELEMC
!------------------------------------------------------------------------------!
subroutine delete_elemc(conn)
implicit none
type(st_elemc) :: conn

  conn%nelem = 0
  if (allocated_elemc(conn)) deallocate(conn%elem)

endsubroutine delete_elemc


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure CONNECT
!------------------------------------------------------------------------------!
subroutine delete_connect(conn)
implicit none
type(st_connect) :: conn

  conn%nbnodes = 0
  if (allocated_connect(conn)) deallocate(conn%fils)

endsubroutine delete_connect


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure GENCONNECT sans nombre de fils
!------------------------------------------------------------------------------!
subroutine new_genconnect_empty(conn, nbnodes)
  implicit none
  type(st_genconnect) :: conn
  integer             :: nbnodes
  integer             :: i

  conn%nbnodes = nbnodes
  allocate(conn%node(nbnodes))
  do i = 1, nbnodes
    conn%node(i)%nelem = 0
  enddo

endsubroutine new_genconnect_empty


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure GENCONNECT avec nombre de fils
!   allocated size is nelem, used size is initdim
!------------------------------------------------------------------------------!
subroutine new_genconnect_nelem(conn, nbnodes, nelem, initdim)
  implicit none
  type(st_genconnect) :: conn
  integer             :: nbnodes, nelem
  integer, optional   :: initdim
  integer             :: i, idim

  if (present(initdim)) then
    idim = initdim
  else
    idim = nelem
  endif
  conn%nbnodes = nbnodes
  allocate(conn%node(nbnodes))
  do i = 1, nbnodes
    conn%node(i)%nelem = idim
    allocate(conn%node(i)%elem(nelem))
  enddo

endsubroutine new_genconnect_nelem

!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure GENCONNECT avec nombre de fils (array)
!------------------------------------------------------------------------------!
subroutine new_genconnect_arraynelem(conn, nbnodes, nelem)
  implicit none
  type(st_genconnect) :: conn
  integer             :: nbnodes, nelem(nbnodes)
  integer             :: i

  conn%nbnodes = nbnodes
  allocate(conn%node(nbnodes))
  do i = 1, nbnodes
    conn%node(i)%nelem = nelem(i)
    allocate(conn%node(i)%elem(nelem(i)))
  enddo

endsubroutine new_genconnect_arraynelem


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure GENCONNECT
!------------------------------------------------------------------------------!
subroutine delete_genconnect(conn)
  implicit none
  type(st_genconnect) :: conn
  integer             :: i

  if (conn%nbnodes /= 0) then
    do i = 1, conn%nbnodes
      if (conn%node(i)%nelem /= 0) deallocate(conn%node(i)%elem)
    enddo
    deallocate(conn%node)
  endif
  conn%nbnodes = 0

endsubroutine delete_genconnect

!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure GENCONNECT avec nombre de fils
!   allocated size is nelem, used size is initdim
!------------------------------------------------------------------------------!
subroutine genconnect_addnode(conn, nelem, initdim)
  implicit none
  type(st_genconnect) :: conn, old
  integer             :: nelem
  integer, optional   :: initdim
  integer             :: i, idim

  if (present(initdim)) then
    idim = initdim
  else
    idim = nelem
  endif
  old = conn                      ! only pointer copy
  conn%nbnodes = old%nbnodes+1
  allocate(conn%node(conn%nbnodes))    ! allocate new elemc array
  do i = 1, old%nbnodes                ! copy(pointers) existing elemc
    conn%node(i) = old%node(i)
  enddo
  deallocate(old%node)
  conn%node(conn%nbnodes)%nelem = idim   ! allocate new elemc
  allocate(conn%node(conn%nbnodes)%elem(nelem))

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : add element "ielem" to inode of genconnect "conn"
!------------------------------------------------------------------------------!
subroutine add_element(conn, inode, ielem, resize)
  implicit none
  type(st_genconnect) :: conn
  integer             :: inode, ielem, i
  integer, optional   :: resize
  type(st_elemc)      :: elemc

  if ((conn%nbnodes /= 0).and.associated(conn%node)) then
    if (allocated_elemc(conn%node(inode))) then             ! --- array already exists ---
      if (size(conn%node(inode)%elem) >= conn%node(inode)%nelem+1) then  ! -- size is sufficient
        conn%node(inode)%nelem = conn%node(inode)%nelem+1                  ! add one element
        conn%node(inode)%elem(conn%node(inode)%nelem) = ielem              ! define element
      else                                                               ! -- need to resize
        call new_elemc_copy(elemc, conn%node(inode))
        call delete_elemc(conn%node(inode))
        if (present(resize)) then
          allocate(conn%node(inode)%elem(elemc%nelem+resize))
        else
          allocate(conn%node(inode)%elem(elemc%nelem+1))
        endif
        conn%node(inode)%nelem = elemc%nelem+1
        forall (i=1:elemc%nelem) conn%node(inode)%elem(i) = elemc%elem(i)
        conn%node(inode)%elem(conn%node(inode)%nelem) = ielem
        call delete_elemc(elemc)
      endif
    else                                                    ! --- array does not exist ---
      if (present(resize)) then
        allocate(conn%node(inode)%elem(resize))
      else
        allocate(conn%node(inode)%elem(1))
      endif
      conn%node(inode)%nelem   = 1
      conn%node(inode)%elem(1) = ielem
    endif
  else
    call cfd_error("cannot add element, connectivity not initialized")
  endif

endsubroutine add_element


!------------------------------------------------------------------------------!
! Fonction : index d'un entier dans une liste d'entiers
!------------------------------------------------------------------------------!
integer function index_int(int, tab)
  implicit none
  integer :: int      ! entier a rechercher
  integer :: tab(:)   ! liste d'entier pour la recherche

  integer :: i, dim

  dim       = size(tab) 
  index_int = 0          ! valeur par defaut si index introuvable

  do i = 1, dim
    if (tab(i)==int) then
      index_int = i
      exit
    endif
  enddo 

endfunction index_int

!------------------------------------------------------------------------------!
! Fonction : index d'un entier dans une liste d'entiers
!------------------------------------------------------------------------------!
integer function elemc_index_int(int, elemc)
  implicit none
  integer        :: int      ! entier a rechercher
  type(st_elemc) :: elemc    ! liste d'entier pour la recherche
  integer :: i, dim

  elemc_index_int = 0          ! valeur par defaut si index introuvable
  do i = 1, elemc%nelem
    if (elemc%elem(i)==int) then
      elemc_index_int = i
      exit
    endif
  enddo 

endfunction elemc_index_int



endmodule CONNECTIVITY

!------------------------------------------------------------------------------!
! History
!
! juil 2003 : creation du module, connectivite simple et generalisee
! juin 2004 : new et delete pour st_elemc
! Oct  2009 : transfered from TYPHON
! May  2013 : search function and add node in genconnect
!------------------------------------------------------------------------------!



