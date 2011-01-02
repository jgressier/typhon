!------------------------------------------------------------------------------!
! Procedure : reorder_ustconnect.f90      Auteur : J. Gressier
!                                         Date   : Fevrier 2003
! Fonction                                Modif  :
!   Renumerotation des faces (faces internes puis faces limites)
!     et des connectivites associees
!
! Defauts/Limitations/Divers :
!   choix des actions a effectuer par (iaction)
!     O: action sur les connectivites, pas le maillage
!     1: action sur connectivites et maillage
!
!------------------------------------------------------------------------------!
subroutine reorder_ustconnect(iaction, umesh) 

use USTMESH       ! Definition des structures maillage non structure

implicit none 

! -- Entrees --
integer             :: iaction

! -- Entrees/Sorties --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Variables internes --
type(st_connect)    :: conn            ! temporary connectivity
integer, dimension(:), allocatable &
                    :: trans_index     ! nouvelle renumerotation
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ntotface        ! nombre total de faces
integer             :: if, oldf, newf

! -- Debut de procedure

if (iaction /= 0) then
  call cfd_error("unexpexted action in reorder_ustconnect")
endif

ntotface = umesh%facecell%nbnodes
allocate(trans_index(ntotface))

! -- renumerotation des faces (faces limites en dernier) --

nface_int = 0
nface_lim = 0

! calcul des nombres de faces et de la transposition d'index
do if = 1, ntotface
  if (umesh%facecell%fils(if,2) == 0) then     ! si face(if) est face limite
    nface_lim = nface_lim + 1
    trans_index(ntotface+1-nface_lim) = if
  else
    nface_int = nface_int + 1
    trans_index(nface_int) = if
  endif
enddo

! --- face reordering ---

conn = copy(umesh%facevtex)
do if = 1, ntotface
  umesh%facevtex%fils(if,:) = conn%fils(trans_index(if),:)
enddo
call delete(conn)

conn = copy(umesh%facecell)
do if = 1, ntotface
  umesh%facecell%fils(if,:) = conn%fils(trans_index(if),:)
enddo
call delete(conn)

!print*,st_allocated(umesh%face_Ltag)
if (st_allocated(umesh%face_Ltag)) then
  conn = copy(umesh%face_Ltag)
  do if = 1, ntotface
    umesh%face_Ltag%fils(if,:) = conn%fils(trans_index(if),:)
  enddo
  call delete(conn)
endif

!print*,st_allocated(umesh%face_Rtag)
if (st_allocated(umesh%face_Rtag)) then
  conn = copy(umesh%face_Rtag)
  do if = 1, ntotface
    umesh%face_Rtag%fils(if,:) = conn%fils(trans_index(if),:)
  enddo
  call delete(conn)
endif

!!$do if = 1, ntotface
!!$  oldf = trans_index(if)
!!$  newf = if
!!$  umesh%facecell%fils(newf,:) = f_cell%fils(oldf,:)
!!$  umesh%facevtex%fils(newf,:) = f_vtex%fils(oldf,:)
!!$enddo

umesh%nface_int = nface_int
umesh%nface_lim = nface_lim

! desallocation

deallocate(trans_index)

!-------------------------
endsubroutine reorder_ustconnect
!------------------------------------------------------------------------------!
! Change history
!
! fev 2003: creation de la procedure
! oct 2007: compact form, added face_L/Rtag reordering
!------------------------------------------------------------------------------!



