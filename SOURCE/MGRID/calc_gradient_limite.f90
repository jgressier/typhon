!------------------------------------------------------------------------------!
! Procedure : calc_gradient_limite        Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des gradients aux limites a partir des gradients uniquement
!
! Defauts/Limitations/Divers :
!   - le calcul des gradient_limites ne doit se faire que sur les cellules limites
!
!------------------------------------------------------------------------------!
subroutine calc_gradient_limite(def_solver, umesh, grad)

use TYPHMAKE
use LAPACK
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD
use USTMESH

implicit none

! -- Declaration des entrees --
type(mnu_solver)      :: def_solver  ! definition des parametres du solveur
type(st_ustmesh)      :: umesh       ! unstructured mesh

! -- Declaration des sorties --
type(st_genericfield) :: grad        ! champ des gradients

! -- Declaration des variables internes --
integer :: nc, nf, nfi, is, if, ic1, ic2

! -- Debut de la procedure --

nc  = umesh%ncell_int   ! nombre de cellules internes
nfi = umesh%nface_int   ! nb de faces internes (connectees avec 2 cellules)
nf  = umesh%nface       ! nb de faces totales 

! calcul des gradient_limites de scalaires (vecteurs gradient)

do is = 1, grad%nvect
  do if = nfi+1, nf
    ic1 = umesh%facecell%fils(if,1)
    ic2 = umesh%facecell%fils(if,2)
    grad%tabvect(is)%vect(ic2) = grad%tabvect(is)%vect(ic1)
  enddo
enddo

! calcul des gradient_limites de vecteurs (tenseur gradient)

do is = 1, grad%ntens
  do if = nfi+1, nf
    ic1 = umesh%facecell%fils(if,1)
    ic2 = umesh%facecell%fils(if,2)
    grad%tabtens(is)%tens(ic2) = grad%tabtens(is)%tens(ic1)
  enddo
enddo

!-----------------------------
endsubroutine calc_gradient_limite

!------------------------------------------------------------------------------!
! Changes history
!
! oct 2003 : created, boundary gradients of scalars
! nov 2004 : add boundary gradients of vectors
!------------------------------------------------------------------------------!
