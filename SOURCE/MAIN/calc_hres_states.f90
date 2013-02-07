!------------------------------------------------------------------------------!
! Procedure : calc_hres_states  
!      
! Fonction  
!   HIGH ORDER EXTRAPOLATION of primitive variables
!
!------------------------------------------------------------------------------!
subroutine calc_hres_states(defsolver, defspat, grid, field)

use OUTPUT
use PACKET
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
integer :: buf                  ! buffer size
integer :: ib, nblock           ! block index and number of blocks
integer, pointer :: ista(:), iend(:)  ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
real(krp) :: klim

! -- BODY --

call new_buf_index(grid%umesh%nface, face_buffer, nblock, ista, iend)

call alloc_hres_states(field)

!$OMP PARALLEL DO private(ib, buf) shared (field, ista, iend)

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  select case(defspat%method)

  case(hres_none)
    
    ! -- no extrapolation, only direct copy of cell values --

    call distrib_field(field%etatprim, grid%umesh%facecell, ista(ib), iend(ib), &
                       field%cell_l, field%cell_r, ista(ib))
  
 
  !----------------------------------------------------------------------
  ! HIGH ORDER states interpolation
  !----------------------------------------------------------------------
  case(hres_muscl)

    call hres_ns_muscl(defspat, buf, ista(ib), grid%umesh,      &
                       field%etatprim, field%gradient,   &
                       field%cell_l, field%cell_r, ista(ib))

  case(hres_musclfast)

    call hres_ns_musclfast(defspat, buf, ista(ib), grid%umesh,      &
                           field%etatprim, field%gradient,   &
                           field%cell_l, field%cell_r, ista(ib))

  case(hres_muscluns)

    call hres_ns_muscluns(defspat, buf, ista(ib), grid%umesh,      &
                          field%etatprim, field%gradient,   &
                          field%cell_l, field%cell_r, ista(ib))

  case(hres_svm)

    call hres_ns_svm(defspat, buf, ista(ib), grid%umesh, field%etatprim, &
                     field%cell_l, field%cell_r, ista(ib))

  case default
    call erreur("High order extrapolation","unknown high resolution method")
  endselect


  !----------------------------------------------------------------------
  ! end of nblock
enddo
!$OMP END PARALLEL DO

deallocate(ista, iend)

!----------------------------------------------------------------------
! POST-LIMITATION
!----------------------------------------------------------------------

select case(defspat%postlimiter)
case(postlim_none)
  ! NOTHING TO DO

case(postlim_barth)
  klim = 0.5_krp
  call postlimit_barth(defspat, klim, grid%umesh, field%etatprim, field%cell_l, field%cell_r)

case(postlim_superbarth)
  klim = 1._krp
  call postlimit_barth(defspat, klim, grid%umesh, field%etatprim, field%cell_l, field%cell_r)

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
