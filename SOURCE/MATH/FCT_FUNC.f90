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
type st_fct_node
  integer(ipar)                 :: type_node
  integer(ipar)                 :: type_oper
  integer(ipar)                 :: size
  type(st_fct_node), pointer :: left, right
endtype st_fct_node


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_fct_node
endinterface

interface delete
  module procedure delete_fct_node
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_fct_node : allocate FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine new_fct_node(mat, dim, ncouple)
implicit none
! - paramètres
type(st_fct_node) :: mat
integer(ipar) :: dim, ncouple


endsubroutine new_fct_node

!------------------------------------------------------------------------------!
! delete_fct_node : remove FCT_FUNC structure
!------------------------------------------------------------------------------!
subroutine delete_fct_node(mat)
implicit none
! - paramètres
type(st_fct_node) :: mat

endsubroutine delete_fct_node


endmodule FCT_FUNC


!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2004 : module creation
!------------------------------------------------------------------------------!
