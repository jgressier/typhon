!------------------------------------------------------------------------------!
! Procedure : integration_kdif_ust              Authors : J. Gressier
!                                               Created : April 2003
! Fonction                                      Modif  : (cf history)
!   Given field and boundary conditions, Computation of
!   - explicit fluxes
!   - jacobian matrices (if needed)
!
!------------------------------------------------------------------------------!
subroutine integration_kdif_ust(defsolver, defspat, umesh, field, flux, &
                                calc_jac, jacL, jacR)

use OUTPUT
use PACKET
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use EQKDIF
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(st_ustmesh) :: umesh            ! unstructured mesh
logical          :: calc_jac         ! choix de calcul de la jacobienne

! -- Inputs/Outputs --
type(st_field)   :: field            ! champ des valeurs et residus

! -- Outputs --
type(st_genericfield)   :: flux        ! flux physiques
type(st_mattab)         :: jacL, jacR  ! jacobiennes associees (gauche et droite)

! -- Internal variables --
integer :: if                   ! face index
integer :: buf                  ! buffer size
integer :: ib, nblock           ! block index and number of blocks
integer, pointer :: ista(:), iend(:)           ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
real(krp), dimension(:), allocatable & 
        :: cell_l, cell_r       ! temperature arrays (left & right cells)
type(v3d), dimension(:), allocatable &
        :: grad_l, grad_r       ! tableau des gradients
type(v3d), dimension(:), allocatable &
        :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- Body --

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL private(ib, cell_l, cell_r, cg_l, cg_r, grad_l, grad_r, buf, icl, icr, if, it) shared (flux, jacL, jacR)

allocate(grad_l(face_buffer), grad_r(face_buffer))
allocate(cell_l(face_buffer), cell_r(face_buffer))
allocate(  cg_l(face_buffer),   cg_r(face_buffer))

!$OMP DO 

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  do it = 1, buf
    if  = ista(ib)+it-1
    icl = umesh%facecell%fils(if,1)
    icr = umesh%facecell%fils(if,2)
    grad_l(it) = field%gradient%tabvect(1)%vect(icl)
    grad_r(it) = field%gradient%tabvect(1)%vect(icr)
    cell_l(it) = field%etatprim%tabscal(1)%scal(icl)
    cell_r(it) = field%etatprim%tabscal(1)%scal(icr)
    cg_l(it)   = umesh%mesh%centre(icl, 1, 1)
    cg_r(it)   = umesh%mesh%centre(icr, 1, 1)
  enddo

  ! - dans une version ulterieure, il sera necessaire de faire intervenir les gradients
  ! - l'acces au tableau flux n'est pas programme de maniere generale !!! DEV

  ! ATTENTION : le flux n'est passe ici que pour UN SEUL scalaire

  call calc_kdif_flux(defsolver, defspat,                             &
                      buf, umesh%mesh%iface(ista(ib):iend(ib), 1, 1),       &
                      cg_l, cell_l, grad_l, cg_r, cell_r, grad_r,     &
                      flux%tabscal(1)%scal(ista(ib):iend(ib)), ista(ib),          &
                      calc_jac, jacL, jacR)
  
enddo

deallocate(grad_l, grad_r, cell_l, cell_r, cg_l, cg_r)

!$OMP END PARALLEL

deallocate(ista, iend)

!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call kdif_bocoflux(defsolver, umesh, flux, field%etatprim,          &
                   calc_jac, jacL, jacR)

endsubroutine integration_kdif_ust
!------------------------------------------------------------------------------!
! Change history
!
! Apr 2003: created
! Jun 2003: update management of conservative and primitive variables
! Oct 2003: gradients added in left and right distribution
! Apr 2004: jacobian matrices computation for implicit solver
! Feb 2011: OPEN-MP directives
!------------------------------------------------------------------------------!
