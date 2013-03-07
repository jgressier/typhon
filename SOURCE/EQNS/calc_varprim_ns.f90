!------------------------------------------------------------------------------!
! Procedure : calc_varprim_ns             Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des variables primitives a partir des variables conservatives
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_varprim_ns(defns, field)

use OUTPUT
use PACKET
use MENU_SOLVER
use DEFFIELD

implicit none

! -- INPUTS --
type(mnu_ns) :: defns       ! definition des parametres du solveur

! -- INPUTS/OUTPUTS --
type(st_field)   :: field       ! champ primitives->conservatives

! -- Private DATA --
integer   :: i, ib
integer   :: ncell
real(krp) :: rho, ec
real(krp) :: g1
type(v3d) :: vel
integer               :: buf, nblock      ! buffer size 
integer, pointer      :: ista(:), iend(:) ! starting and ending index

! -- BODY --

ncell = field%ncell    ! compute on all cells (ghost cells too even if not necessary)
g1    = defns%properties(1)%gamma - 1._krp

call new_buf_index(ncell, cell_buffer, nblock, ista, iend)

!$OMP PARALLEL DO private(rho, vel, ec, buf) shared(field)
do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  do i = ista(ib), iend(ib)
    rho = field%etatcons%tabscal(1)%scal(i)
    vel = field%etatcons%tabvect(1)%vect(i) / rho
    ec  = .5_krp*rho*sqrabs(vel)
    field%etatprim%tabscal(1)%scal(i) = rho
    field%etatprim%tabscal(2)%scal(i) = g1*(field%etatcons%tabscal(2)%scal(i) - ec)
    field%etatprim%tabvect(1)%vect(i) = vel
  enddo
enddo
!$OMP END PARALLEL DO

deallocate(ista, iend)

!-----------------------------
endsubroutine calc_varprim_ns
!------------------------------------------------------------------------------!
! History
!
! oct  2003 : creation
! july 2004 : actual computations
! Mar  2011 : split into buffer packets
!------------------------------------------------------------------------------!
