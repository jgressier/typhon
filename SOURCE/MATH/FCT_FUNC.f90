!------------------------------------------------------------------------------!
! MODULE : FCT_FUNC                       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see history)
!   Definition of FUNCTION
!
!------------------------------------------------------------------------------!

module FCT_FUNC

implicit none

! -- Constants -------------------------------------------


     
! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FCT_FUNC : 
!------------------------------------------------------------------------------!
type st_fct_func

endtype st_fct_func


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_fct_func
endinterface

interface delete
  module procedure delete_fct_func
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_func : allocate FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine new_fct_func(mat, dim, ncouple)
implicit none
! - paramètres
type(st_fct_func) :: mat
integer(ipar) :: dim, ncouple


endsubroutine new_fct_func

!------------------------------------------------------------------------------!
! delete_fct_func : remove FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine delete_fct_func(mat)
implicit none
! - paramètres
type(st_fct_func) :: mat

endsubroutine delete_fct_func


endmodule FCT_FUNC


!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2004 : module creation
!------------------------------------------------------------------------------!
