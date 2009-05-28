!------------------------------------------------------------------------------!
! Procedure : calc_hres_states  
!      
! Fonction  
!   HIGH ORDER EXTRAPOLATION of primitive variables
!
!------------------------------------------------------------------------------!
subroutine calc_hres_states(defsolver, defspat, grid, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD

implicit none

! -- Inputs --
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(st_grid)    :: grid             ! 

! -- Inputs/Outputs --
type(st_field)   :: field            ! use field%etatprim to create field%cell_l, field%cell_r

! -- Internal variables --
logical :: gradneeded           ! use gradients or not
integer :: if                   ! face index
integer :: buf, dimbuf, dimbuf1 ! buffer size (current, regular and first)
integer :: ib, nblock           ! block index and number of blocks
integer :: ista, iend           ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite

! -- BODY --

call calc_buffer(grid%umesh%nface, cell_buffer, nblock, dimbuf, dimbuf1)

call alloc_hres_states(field, grid%umesh%nface)

ista = 1
buf  = dimbuf1

do ib = 1, nblock

  iend = ista+buf-1

  select case(defspat%method)

  case(hres_none)
    
    ! -- no extrapolation, only direct copy of cell values --

    call distrib_field(field%etatprim, grid%umesh%facecell, ista, iend, &
                       field%cell_l, field%cell_r, ista)
  
 
  !----------------------------------------------------------------------
  ! HIGH ORDER states interpolation
  !----------------------------------------------------------------------
  case(hres_muscl)

    call hres_ns_muscl(defspat, buf, ista, grid%umesh,      &
                       field%etatprim, field%gradient,   &
                       field%cell_l, field%cell_r, ista)

  case(hres_musclfast)

    call hres_ns_musclfast(defspat, buf, ista, grid%umesh,      &
                           field%etatprim, field%gradient,   &
                           field%cell_l, field%cell_r, ista)

  case(hres_muscluns)

    call hres_ns_muscluns(defspat, buf, ista, grid%umesh,      &
                          field%etatprim, field%gradient,   &
                          field%cell_l, field%cell_r, ista)

  case(hres_svm)

    call hres_ns_svm(defspat, buf, ista, grid%umesh, field%etatprim, &
                     field%cell_l, field%cell_r, ista)

  case default
    call erreur("High order extrapolation","unknown high resolution method")
  endselect


  !----------------------------------------------------------------------
  ! end of nblock

  ista = ista + buf
  buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf
  
enddo

!----------------------------------------------------------------------
! POST-LIMITATION
!----------------------------------------------------------------------

select case(defspat%postlimiter)
case(postlim_none)
  ! NOTHING TO DO

case(postlim_barth)
  call postlimit_barth(defspat, grid%umesh, field%etatprim, field%cell_l, field%cell_r)

case(postlim_monotonic0, postlim_monotonic1, postlim_monotonic2)
  call postlimit_monotonic(defspat, grid%umesh, field%etatprim, field%cell_l, field%cell_r)

case default
  call erreur("High order extrapolation","unknown POST-LIMITATION method")
endselect

endsubroutine calc_hres_states

!------------------------------------------------------------------------------!
! Changes history
!
! May  2009: new routine, extracted high order extrapolation from integration_ns_ust
!------------------------------------------------------------------------------!
