!------------------------------------------------------------------------------!
! Procedure : calc_gradient_limite        Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des gradients aux limites à partir des gradients uniquement
!
! Defauts/Limitations/Divers :
!   - le calcul des gradient_limites ne doit se faire que sur les cellules limites
!
!------------------------------------------------------------------------------!
subroutine calc_gradient_limite(def_solver, mesh, grad)

use TYPHMAKE
use LAPACK
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD
use USTMESH

implicit none

! -- Declaration des entrées --
type(mnu_solver)      :: def_solver  ! définition des paramètres du solveur
type(st_ustmesh)      :: mesh        ! maillage et connectivités

! -- Declaration des sorties --
type(st_genericfield) :: grad        ! champ des gradients

! -- Declaration des variables internes --
integer :: nc, nf, nfi, is, if, ic1, ic2

! -- Debut de la procedure --

nc  = mesh%ncell_int   ! nombre de cellules internes
nfi = mesh%nface_int   ! nb de faces internes (connectées avec 2 cellules)
nf  = mesh%nface       ! nb de faces totales 

! calcul des gradient_limites de scalaires (vecteurs gradient)

do is = 1, grad%nvect
  do if = nfi+1, nf
    ic1 = mesh%facecell%fils(if,1)
    ic2 = mesh%facecell%fils(if,2)
    grad%tabvect(is)%vect(ic2) = grad%tabvect(is)%vect(ic1)
  enddo
enddo

! calcul des gradient_limites de vecteurs (tenseur gradient)

do is = 1, grad%ntens
  call erreur("Développement","calcul de gradient_limites de vecteurs non implémenté")
enddo

!-----------------------------
endsubroutine calc_gradient_limite

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct 2003 : création de la procédure
!------------------------------------------------------------------------------!
