!------------------------------------------------------------------------------!
! Procedure : calc_varcons_ns             Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des variables conservatives à partir des variables primitives
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_varcons_ns(defns, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use DEFFIELD

implicit none

! -- Declaration des entrées --
type(mnu_ns) :: defns       ! définition des paramètres du solveur

! -- Declaration des entrées/sorties --
type(st_field)   :: field       ! champ primitives->conservatives

! -- Declaration des variables internes --
integer   :: i
integer   :: ncell
real(krp) :: rho
real(krp) :: ig1
type(v3d) :: vel

! -- Debut de la procedure --

ncell = field%ncell
ig1   = defns%properties(1)%gamma - 1._krp

do i = 1, ncell
  rho = field%etatprim%tabscal(1)%scal(i)
  vel = field%etatprim%tabvect(1)%vect(i) 
  field%etatcons%tabscal(1)%scal(i) = rho
  field%etatcons%tabscal(2)%scal(i) = ig1*field%etatprim%tabscal(2)%scal(i) + &
                                      .5_krp*rho*sqrabs(vel)
  field%etatcons%tabvect(1)%vect(i) = rho*vel
enddo

!-----------------------------
endsubroutine calc_varcons_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2003 : création de la procédure
! july 2004 : actual computations
!------------------------------------------------------------------------------!
