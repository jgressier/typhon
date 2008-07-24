!------------------------------------------------------------------------------!
! Procedure : dlu_lu                      Authors : J. Gressier
!                                         Created : Avril 2004
! Fonction
!   Resolution d'un systeme lineaire mat.sol = rhs
!     mat sous forme type(st_dlu)
!     methode directe : decomposition LU et resolution
!
! Defauts/Limitations/Divers :
!   - Array sol(*) contains rhs as input
!   - Resolution needs allocation of a full (dim*dim) matrice
!
!------------------------------------------------------------------------------!
subroutine dlu_lu(mat, sol)

use TYPHMAKE
use SPARSE_MAT
use LAPACK

implicit none

! -- Inputs --
type(st_dlu)  :: mat

! -- Inputs/Outputs --
real(krp)     :: sol(1:mat%dim)  ! RHS as input, SOLUTION as output

! -- Outputs --

! -- Internal variables --
real(krp),  dimension(:,:), allocatable :: pmat, pvec
integer(kip), dimension(:), allocatable :: piv
integer(kip)                            :: i, imin, imax, info

! -- Debut de la procedure --


! -- reconstruction d'une matrice pleine --

allocate(pmat(mat%dim, mat%dim))

do i = 1, mat%dim
  pmat(i,i) = mat%diag(i)
enddo

do i = 1, mat%ncouple
  imin = minval(mat%couple%fils(i,1:2))
  imax = maxval(mat%couple%fils(i,1:2))
  if (imax <= mat%dim) then
    pmat(imax,imin) = mat%lower(i)
    pmat(imin,imax) = mat%upper(i)
  endif
enddo

!print*,real(pmat(:,:),4)

! -- reconstruction du vecteur rhs --

allocate(pvec(1:mat%dim, 1))
pvec(1:mat%dim, 1) = sol(1:mat%dim)

! -- resolution --

allocate(piv(mat%dim))

! decomposition LU dans pmat

call erreur("development",&
            "dense LU decomposition no longer available")

!call lapack_getrf(mat%dim, mat%dim, pmat, mat%dim, piv, info)

if (info /= 0) call erreur("resolution matricielle",&
                           "probleme dans la decomposition LU")

! resolution RHS dans pvec en entree, solution en sortie
!call lapack_getrs('N', mat%dim, 1, pmat, mat%dim, piv, pvec, mat%dim, info)

if (info /= 0) call erreur("resolution matricielle",&
                           "probleme dans l'inversion")

! -- redistribution --
sol(1:mat%dim) = pvec(1:mat%dim, 1)

! desallocation

deallocate(pmat, pvec, piv)


endsubroutine dlu_lu

!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2004 : creation
! oct  2006 : stopped in order to avoid LAPACK library (dense LU not efficient)
!------------------------------------------------------------------------------!
