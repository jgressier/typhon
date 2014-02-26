!------------------------------------------------------------------------------!
! Procedure : calc_gradient               Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des gradients d'un champ generique (conservatif ou primitif)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_gradient(defsolver, defspat, grid, gfield, grad)

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
real(krp), allocatable :: dcg(:,:)             ! delta cg
real(krp), allocatable :: rhs(:,:), prhs(:,:)  ! second membre
real(krp)              :: dsca        ! variation de variable scalaire
type(v3d)              :: dvec        ! variation de variable vectorielle
integer                :: i, ic, nc   ! indice et nombre de cellules internes
integer                :: dec         ! index shifting (pack & unpack)
integer                :: dim         ! full state size
integer                :: if, nf, nfi ! indice et nombre de faces totales et internes
integer                :: nv          ! nombre de variables
integer                :: is, iv      ! indice de variable scalaire et vectorielle
integer                :: ic1, ic2    ! indices de cellules (gauche et droite de face)
integer                :: info, xinfo ! retour d'info des routines LAPACK
integer, pointer       :: ifsta(:), ifend(:)     ! starting and ending index
integer, pointer       :: icsta(:), icend(:)     ! starting and ending index
integer                :: icolor
integer                :: ifb, fbuf, nfblock     ! buffer size for face
integer                :: icb, cbuf, ncblock     ! buffer size 

! -- BODY --

if (.not.grid%optmem%gradcond_computed) then
  call precalc_grad_lsq(defsolver, defspat, grid)
endif

! need OPTIMIZATION
! - splitting of loops into packets
! - define some calls (check efficiency)
! - memorize geometrical matrix


! -- Calcul du second membre -At.dT et resolution (multiplication par l'inverse) --
! ?? OPTIMISATION par interface variable/tableaux et resolution en un seul coup

nc  = grid%umesh%ncell_int   ! nombre de cellules internes
nfi = grid%umesh%nface_int   ! nb de faces internes (connectees avec 2 cellules)
nf  = grid%umesh%nface       ! nb de faces totales 
dim = gfield%nscal+3*gfield%nvect

allocate(dcg(3,nf))
allocate(rhs(3,nc*dim))    ! allocation

select case(defspat%gradmeth)
case(cellgrad_lsq)
  !$OMP PARALLEL DO private(if, ic1, ic2)
  do if = 1, nf
    ic1  = grid%umesh%facecell%fils(if,1)
    ic2  = grid%umesh%facecell%fils(if,2)
    dcg(1:3,if) = tab(grid%umesh%mesh%centre(ic2,1,1) - grid%umesh%mesh%centre(ic1,1,1))
  enddo
  !$OMP END PARALLEL DO
case(cellgrad_lsqw)
  !$OMP PARALLEL DO private(if, ic1, ic2, dvec)
  do if = 1, nf
    ic1  = grid%umesh%facecell%fils(if,1)
    ic2  = grid%umesh%facecell%fils(if,2)
    dvec = grid%umesh%mesh%centre(ic2,1,1) - grid%umesh%mesh%centre(ic1,1,1)
    dcg(1:3,if) = tab(dvec)/sqrabs(dvec)**2
  enddo
  !$OMP END PARALLEL DO
case default
  call error_stop("Internal error: unknown GRADIENT computation method")
endselect

!-----------------------------------------------------------------------------
! Fill RHS and pack scalars and vectors
!-----------------------------------------------------------------------------

rhs(:,:) = 0._krp      ! initialisation

! Calcul des seconds membres (cellules internes et limites)

do icolor = 1, grid%umesh%colors%nbnodes

!$OMP PARALLEL DO &
!$  private(ic1, ic2, if, is, iv, dec, dsca, i) &
!$  shared(rhs, nfi) 
do i = 1, grid%umesh%colors%node(icolor)%nelem

  if  = grid%umesh%colors%node(icolor)%elem(i)
  ic1 = grid%umesh%facecell%fils(if,1)
  ic2 = grid%umesh%facecell%fils(if,2)

  do is = 1, gfield%nscal
    dec = is-dim
    dsca = gfield%tabscal(is)%scal(ic2) - gfield%tabscal(is)%scal(ic1) 
    rhs(1:3, ic1*dim+dec) = rhs(1:3, ic1*dim+dec) + dsca*dcg(1:3,if)
    if (if <= nfi) then
      rhs(1:3, ic2*dim+dec) = rhs(1:3, ic2*dim+dec) + dsca*dcg(1:3,if)
    endif
  enddo

  do iv = 1, gfield%nvect
    ! -- X component --
    dec = gfield%nscal-dim + 3*(iv-1) +1
    dsca = gfield%tabvect(iv)%vect(ic2)%x - gfield%tabvect(iv)%vect(ic1)%x
    rhs(1:3, ic1*dim+dec) = rhs(1:3, ic1*dim+dec) + dsca*dcg(1:3,if)
    if (if <= nfi) then
      rhs(1:3, ic2*dim+dec) = rhs(1:3, ic2*dim+dec) + dsca*dcg(1:3,if)
    endif
    ! -- Y component --
    dec = gfield%nscal-dim + 3*(iv-1) +2
    dsca = gfield%tabvect(iv)%vect(ic2)%y - gfield%tabvect(iv)%vect(ic1)%y
    rhs(1:3, ic1*dim+dec) = rhs(1:3, ic1*dim+dec) + dsca*dcg(1:3,if)
    if (if <= nfi) then
      rhs(1:3, ic2*dim+dec) = rhs(1:3, ic2*dim+dec) + dsca*dcg(1:3,if)
    endif
    ! -- Z component --
    dec = gfield%nscal-dim + 3*(iv-1) +3
    dsca = gfield%tabvect(iv)%vect(ic2)%z - gfield%tabvect(iv)%vect(ic1)%z
    rhs(1:3, ic1*dim+dec) = rhs(1:3, ic1*dim+dec) + dsca*dcg(1:3,if)
    if (if <= nfi) then
      rhs(1:3, ic2*dim+dec) = rhs(1:3, ic2*dim+dec) + dsca*dcg(1:3,if)
    endif
  enddo

enddo
!$OMP END PARALLEL DO
enddo ! color

!-----------------------------------------------------------------------------
! Solve LSQ
!-----------------------------------------------------------------------------

info  = 0
xinfo = 0

!$OMP PARALLEL private(ic, prhs)
allocate(prhs(3,dim))
!$OMP DO
do ic = 1, nc
  prhs = rhs(1:3, (ic-1)*dim+1:ic*dim)
  call cholesky_solve(grid%optmem%gradcond(ic)%mat, 3, prhs, dim)
  rhs(1:3, (ic-1)*dim+1:ic*dim) = prhs
enddo
!$OMP END DO
deallocate(prhs)
!$OMP END PARALLEL 

!if (xinfo /= 0) call erreur("Gradient computation","Choleski inversion failed")

!-----------------------------------------------------------------------------
! Unpack solution
!-----------------------------------------------------------------------------

!$OMP PARALLEL DO private (ic, dec, is, iv)
do ic = 1, nc
  dec = (ic-1)*dim
  do is = 1, gfield%nscal
    grad%tabvect(is)%vect(ic)%x = rhs(1,dec+is)
    grad%tabvect(is)%vect(ic)%y = rhs(2,dec+is)
    grad%tabvect(is)%vect(ic)%z = rhs(3,dec+is)
  enddo
  do iv = 1, gfield%nvect
    grad%tabtens(iv)%tens(ic)%mat(1,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+1)
    grad%tabtens(iv)%tens(ic)%mat(2,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+2)
    grad%tabtens(iv)%tens(ic)%mat(3,1:3) = rhs(1:3,dec+gfield%nscal+(iv-1)*3+3)
  enddo
enddo
!$OMP END PARALLEL DO

! --desallocation

deallocate(dcg, rhs)


!-----------------------------
endsubroutine calc_gradient

!------------------------------------------------------------------------------!
! Changes history
!
! sept 2003 : creation de la procedure
! nov  2004 : computation of vector gradients
! oct  2005 : restructuration (pack scalar & vectors for a single LSQ solve)
! May  2013 : colored OMP
!------------------------------------------------------------------------------!
