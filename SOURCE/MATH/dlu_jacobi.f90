!------------------------------------------------------------------------------!
! Procedure : dlu_jacobi                  Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Resolution d'un systeme lineaire mat.sol = rhs
!     mat sous forme type(st_dlu)
!     methode iterative JACOBI : mat = D + L + U
!       sol(n+1) = D^(-1).-(L+U).sol(n) + D^(-1).rhs
!
! Defauts/Limitations/Divers :
!   - le tableau sol(*) est cense etre deja alloue
!   - la resolution passe par l'allocation d'une matrice pleine (dim*dim)
!
!------------------------------------------------------------------------------!
subroutine dlu_jacobi(def_impli, mat, rhs, sol, info)

use TYPHMAKE
use SPARSE_MAT
use LAPACK
use MENU_NUM

implicit none

! -- Declaration des entrees --
type(mnu_imp) :: def_impli
type(st_dlu)  :: mat
real(krp)     :: rhs(1:mat%dim)

! -- Declaration des sorties --
real(krp)     :: sol(1:mat%dim)
integer(kip)  :: info

! -- Declaration des variables internes --
real(krp), dimension(:), allocatable :: vec, soln
integer(kip)                         :: nit, ic, if, imin, imax
real(krp)                            :: erreur, ref

! -- Debut de la procedure --

! initialisation

nit    = 0
erreur = huge(erreur)    ! maximal real number in machine representation (to ensure 1st iteration)

allocate( vec(mat%dim))
allocate(soln(mat%dim))

soln(1:mat%dim) = rhs(1:mat%dim) / mat%diag(1:mat%dim)
ref = sum(abs(soln(:)))

!call sort_dlu(mat)

do while ((erreur >= ref*def_impli%maxres).and.(nit <= def_impli%max_it))

  vec(1:mat%dim) = rhs(1:mat%dim)

  ! multiplication par (L + U)

  do if = 1, mat%ncouple
    imin = mat%couple%fils(if,1) ! ic1 cell is supposed to be the lowest index
    imax = mat%couple%fils(if,2) ! ic2 cell is supposed to be the highest index
    if (imax <= mat%dim) then
      vec(imax) = vec(imax) - mat%lower(if)*soln(imin)
      vec(imin) = vec(imin) - mat%upper(if)*soln(imax)
    endif
  enddo

  ! division par D (coef par coef)

  vec(1:mat%dim) = vec(1:mat%dim) / mat%diag(1:mat%dim)

  ! calcul de l'erreur

  erreur  = sum(abs(soln(:)-vec(:)))
  !print*,'conv jacobi',nit,log10(erreur/ref)
  soln(:) = vec(:)   
  nit     = nit + 1

enddo

sol(:) = soln(:)
if (nit <= def_impli%max_it) then
  info = nit - 1
else
  info = -1
endif

deallocate(vec, soln)

endsubroutine dlu_jacobi

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : creation de la procedure
!------------------------------------------------------------------------------!
