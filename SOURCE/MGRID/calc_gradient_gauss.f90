!------------------------------------------------------------------------------!
! Procedure : calc_gradient_gauss              
!                                         
!> @brief Calcul des gradients d'un champ generique Par Gauss theorem
!------------------------------------------------------------------------------!
subroutine calc_gradient_gauss(defsolver, defspat, grid, gfield, grad)

use TYPHMAKE
use LAPACK
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use DEFFIELD
use MGRID
use MATRIX

implicit none

! -- INPUTS --
type(mnu_solver)      :: defsolver   ! definition des parametres du solveur
type(mnu_spat)        :: defspat     ! spatial numerical parameters
type(st_grid)         :: grid        ! maillage et connectivites
type(st_genericfield) :: gfield      ! champ des valeurs 

! -- OUTPUTS --
type(st_genericfield) :: grad        ! champ des gradients

! -- Internal variables --
real(krp), allocatable :: rhs(:,:)    ! second membre
real(krp)              :: surf        ! surface
real(krp)              :: vol         ! vol
type(v3d)              :: fn          ! normale
integer                :: ic, nc      ! indice et nombre de cellules internes
integer                :: dec         ! index shifting (pack & unpack)
integer                :: dim         ! full state size
integer                :: if, nf, nfi ! indice et nombre de faces totales et internes
integer                :: nv          ! nombre de variables
integer                :: is, iv      ! indice de variable scalaire et vectorielle
integer                :: ic1, ic2    ! indices de cellules (gauche et droite de face)

! -- BODY --


! need OPTIMIZATION
! - splitting of loops into packets
! - define some calls (check efficiency)


! ?? OPTIMISATION par interface variable/tableaux et resolution en un seul coup

nc  = grid%umesh%ncell_int   ! nombre de cellules internes
nfi = grid%umesh%nface_int   ! nb de faces internes (connectees avec 2 cellules)
nf  = grid%umesh%nface       ! nb de faces totales 
dim = gfield%nscal+3*gfield%nvect


allocate(rhs(3,nc*dim))    ! allocation


!-----------------------------------------------------------------------------
! Fill RHS and pack scalars and vectors
!-----------------------------------------------------------------------------

rhs(:,:) = 0._krp      ! initialisation


!!$OMP PARALLEL DO private(if, ic1, ic2, is, iv, dec) shared (rhs)   ! , reduction(+: rhs)

do if = 1, nf

  ic1  = grid%umesh%facecell%fils(if,1)
  ic2  = grid%umesh%facecell%fils(if,2)
  surf = grid%umesh%mesh%face_surf(if) 
  fn = grid%umesh%mesh%face_normal(if,1)
  do is = 1, gfield%nscal
    dec = is-dim
    rhs(1, ic1*dim+dec) = rhs(1, ic1*dim+dec) + gfield%tabscal(is)%scal(ic1) * fn%x * surf
    rhs(2, ic1*dim+dec) = rhs(2, ic1*dim+dec) + gfield%tabscal(is)%scal(ic1) * fn%y * surf
    rhs(3, ic1*dim+dec) = rhs(3, ic1*dim+dec) + gfield%tabscal(is)%scal(ic1) * fn%z * surf
    if (if <= nfi) then
      rhs(1, ic2*dim+dec) = rhs(1, ic2*dim+dec) + gfield%tabscal(is)%scal(ic2) * fn%x * surf
      rhs(2, ic2*dim+dec) = rhs(2, ic2*dim+dec) + gfield%tabscal(is)%scal(ic2) * fn%y * surf
      rhs(3, ic2*dim+dec) = rhs(3, ic2*dim+dec) + gfield%tabscal(is)%scal(ic2) * fn%z * surf
    endif
  enddo

  do iv = 1, gfield%nvect
    ! -- X component --
    dec = gfield%nscal-dim + 3*(iv-1) +1
    rhs(1, ic1*dim+dec) = rhs(1, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%x * fn%x  * surf
    rhs(2, ic1*dim+dec) = rhs(2, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%x * fn%y  * surf
    rhs(3, ic1*dim+dec) = rhs(3, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%x * fn%z  * surf
    if (if <= nfi) then
    rhs(1, ic2*dim+dec) = rhs(1, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%x * fn%x * surf
    rhs(2, ic2*dim+dec) = rhs(2, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%x * fn%y * surf
    rhs(3, ic2*dim+dec) = rhs(3, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%x * fn%z * surf
    endif
    ! -- Y component --
    dec = gfield%nscal-dim + 3*(iv-1) +2
    rhs(1, ic1*dim+dec) = rhs(1, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%y * fn%x  * surf
    rhs(2, ic1*dim+dec) = rhs(2, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%y * fn%y  * surf
    rhs(3, ic1*dim+dec) = rhs(3, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%y * fn%z  * surf
    if (if <= nfi) then
    rhs(1, ic2*dim+dec) = rhs(1, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%y * fn%x * surf
    rhs(2, ic2*dim+dec) = rhs(2, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%y * fn%y * surf
    rhs(3, ic2*dim+dec) = rhs(3, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%y * fn%z * surf
    endif
    ! -- Z component --
    dec = gfield%nscal-dim + 3*(iv-1) +3
    rhs(1, ic1*dim+dec) = rhs(1, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%z * fn%x  * surf
    rhs(2, ic1*dim+dec) = rhs(2, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%z * fn%y  * surf
    rhs(3, ic1*dim+dec) = rhs(3, ic1*dim+dec) + gfield%tabvect(iv)%vect(ic1)%z * fn%z  * surf
    if (if <= nfi) then
    rhs(1, ic2*dim+dec) = rhs(1, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%z * fn%x  * surf
    rhs(2, ic2*dim+dec) = rhs(2, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%z * fn%y  * surf
    rhs(3, ic2*dim+dec) = rhs(3, ic2*dim+dec) + gfield%tabvect(iv)%vect(ic2)%z * fn%z  * surf
    endif
  enddo

enddo

!!$OMP END PARALLEL DO


!$OMP PARALLEL DO private (ic, dec, is, iv)

do ic = 1, nc
  dec = (ic-1)*dim
  vol = grid%umesh%mesh%volume(ic,1,1)
  do is = 1, gfield%nscal
    grad%tabvect(is)%vect(ic) = v3d_of(rhs(1:3,dec+is))/vol
  enddo
  do iv = 1, gfield%nvect
    grad%tabtens(iv)%tens(ic)%mat(1,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+1)/vol
    grad%tabtens(iv)%tens(ic)%mat(2,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+2)/vol
    grad%tabtens(iv)%tens(ic)%mat(3,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+3)/vol
  enddo
enddo

!$OMP END PARALLEL DO

! --desallocation

deallocate(rhs)


!-----------------------------
endsubroutine calc_gradient_gauss

!------------------------------------------------------------------------------!
! Changes history
!
!
!------------------------------------------------------------------------------!
