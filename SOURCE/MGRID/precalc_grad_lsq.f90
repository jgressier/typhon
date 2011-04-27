!------------------------------------------------------------------------------!
! Procedure : precalc_grad_lsq            Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des gradients d'un champ generique (conservatif ou primitif)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine precalc_grad_lsq(defsolver, defspat, grid)

use TYPHMAKE
use LAPACK
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use DEFFIELD
use USTMESH
use MATRIX
use TENSOR3

implicit none

! -- INPUTS --
type(mnu_solver)      :: defsolver  ! definition des parametres du solveur
type(mnu_spat)        :: defspat     ! spatial numerical parameters
type(st_grid)         :: grid        ! maillage et connectivites

! -- Internal parameters --
type(v3d), allocatable :: dcg(:)      ! delta cg
!real(krp), allocatable :: rhs(:,:)    ! second membre
real(krp)              :: imat(3,3)   ! matrice locale
integer                :: ic, nc      ! indice et nombre de cellules internes
integer                :: if, nf, nfi ! indice et nombre de faces totales et internes
integer                :: nv          ! nombre de variables
integer                :: is, iv      ! indice de variable scalaire et vectorielle
integer                :: ic1, ic2    ! indices de cellules (gauche et droite de face)
integer                :: info, xinfo ! retour d'info des routines LAPACK

! -- BODY --

nc  = grid%umesh%ncell_int   ! nombre de cellules internes
nfi = grid%umesh%nface_int   ! nb de faces internes (connectees avec 2 cellules)
nf  = grid%umesh%nface       ! nb de faces totales 
allocate(dcg(nf))

! need OPTIMIZATION
! - splitting of loops into packets
! - define some calls (check efficiency)
! - memorize geometrical matrix

call grid_alloc_gradcond(grid)  ! automatic testing of allocation

grid%optmem%gradcond_computed = .true.

! -- Calcul des differences de centres de cellules --
!    (toutes les faces, meme limites, doivent avoir un centre de cellule)

select case(defspat%gradmeth)
case(grad_lsq)
  do if = 1, nf
    ic1 = grid%umesh%facecell%fils(if,1)
    ic2 = grid%umesh%facecell%fils(if,2)
    dcg(if) = grid%umesh%mesh%centre(ic2,1,1) - grid%umesh%mesh%centre(ic1,1,1) 
  enddo
case(grad_lsqw)
  do if = 1, nf
    ic1  = grid%umesh%facecell%fils(if,1)
    ic2  = grid%umesh%facecell%fils(if,2)
    dcg(if) = grid%umesh%mesh%centre(ic2,1,1) - grid%umesh%mesh%centre(ic1,1,1) 
    dcg(if) = dcg(if)/sqrabs(dcg(if))
  enddo
case default
  call erreur("Internal error", "unknown GRADIENT computation method")
endselect


! -- Calcul des matrices At.A et inversion --

do ic = 1, nc
  grid%optmem%gradcond(ic)%mat = 0._krp
enddo

! -- boucle sur les faces internes uniquement (code source double)  --

do if = 1, nfi
  imat(1,1) = dcg(if)%x **2
  imat(2,2) = dcg(if)%y **2
  imat(3,3) = dcg(if)%z **2
  imat(1,2) = dcg(if)%x * dcg(if)%y
  imat(2,1) = imat(1,2)
  imat(1,3) = dcg(if)%x * dcg(if)%z
  imat(3,1) = imat(1,3)
  imat(2,3) = dcg(if)%y * dcg(if)%z
  imat(3,2) = imat(2,3)
  
  ! contribution de la face a la cellule a gauche
  ic1 = grid%umesh%facecell%fils(if,1)
  grid%optmem%gradcond(ic1)%mat(:,:) = grid%optmem%gradcond(ic1)%mat(:,:) + imat(:,:)
  
  ! contribution de la face a la cellule a droite
  ic2 = grid%umesh%facecell%fils(if,2)
  grid%optmem%gradcond(ic2)%mat(:,:) = grid%optmem%gradcond(ic2)%mat(:,:) + imat(:,:)
enddo

! -- boucle sur les faces limites uniquement (code source double) --

do if = nfi+1, nf
  imat(1,1) = dcg(if)%x **2
  imat(2,2) = dcg(if)%y **2
  imat(3,3) = dcg(if)%z **2
  imat(1,2) = dcg(if)%x * dcg(if)%y
  imat(2,1) = imat(1,2)
  imat(1,3) = dcg(if)%x * dcg(if)%z
  imat(3,1) = imat(1,3)
  imat(2,3) = dcg(if)%y * dcg(if)%z
  imat(3,2) = imat(2,3)
  
  ! contribution de la face a la cellule a gauche (UNIQUEMENT)
  ic1 = grid%umesh%facecell%fils(if,1)
  grid%optmem%gradcond(ic1)%mat(:,:) = grid%optmem%gradcond(ic1)%mat(:,:) + imat(:,:)
enddo

! -- Correction de la matrice dans les cas 2D (vecteur supplementaire selon z) --

select case(geodim(grid%umesh))
case(2)
  do ic = 1, nc
    ! invariance direction ? magnitude ? !! DEV
    grid%optmem%gradcond(ic)%mat(3,3) = grid%optmem%gradcond(ic)%mat(3,3) + 1._krp  
  enddo
case(3)
  ! nothing to do 
case default
  call error_stop("computing gradients: unknown type of mesh")
endselect

! l'inversion peut se faire des façons suivantes selon A symetrique (PO) ou non (GE)
! * calcul de l'inverse Ai (GETRI/POTRI) et multiplication Ai.B
! * decomposition LU (GETRF) et resolution (GETRS)
! * decomposition Choleski (POTRF) et resolution (POTRS)

info  = 0
xinfo = 0
do ic = 1, nc
  ! decomposition de Choleski
  !call lapack_potrf('U', 3, grid%optmem%gradcond(ic)%mat, 3, info)
  call cholesky_decomp(grid%optmem%gradcond(ic)%mat, 3)
  if (info /= 0) xinfo = ic
  !if (info /= 0) then
  !  xinfo = ic
  !  print*,"DEBUG!!: xinfo = ",xinfo
  !endif
enddo

if (xinfo /= 0) call erreur("Gradient computation","Choleski decomposition failed")

deallocate(dcg)


!-----------------------------
endsubroutine precalc_grad_lsq

!------------------------------------------------------------------------------!
! Changes history
!
! dec  2004 : created (from split of calc_gradient)
! feb  2005 : use grid%optmem array (memorize computation)
!------------------------------------------------------------------------------!
