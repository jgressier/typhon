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
integer :: ip

! -- Debut de la procedure --

do ip = 1, field%nscal

enddo


!-----------------------------
endsubroutine calc_varcons_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2003 : création de la procédure
!------------------------------------------------------------------------------!
