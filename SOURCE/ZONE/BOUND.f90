!------------------------------------------------------------------------------!
! MODULE : BOUND                          Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  :
!   Bibliotheque de procedures et fonctions pour la gestion des champs
!   des differents solveurs
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module BOUND

use TYPHMAKE   ! Definition de la precision
!use EQNS      ! Definition des champs pour equations de Navier-Stokes
use EQKDIF      ! Definition des champs pour equations de diffusion

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------



!------------------------------------------------------------------------------!
! Definition de la structure ST_BOUND : Champ physique 
!------------------------------------------------------------------------------!

type st_bound
  integer      :: idim, jdim, kdim       ! indices max des cellules 
                                                              ! n dimension spatiale
endtype st_bound



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_bound
endinterface

interface delete
  module procedure delete_bound
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure BOUND
!------------------------------------------------------------------------------!
subroutine new_bound(bound, idim, jdim, kdim)
implicit none
type(st_bound) :: bound             ! champ a creer
integer        :: idim, jdim, kdim  ! dimension du champ interne
logical        :: allocgrad         ! allocation des gradients
integer        :: imin, jmin, kmin
integer        :: imax, jmax, kmax

  bound%idim     = idim
  bound%jdim     = jdim
  bound%kdim     = kdim

endsubroutine new_bound


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure BOUND
!------------------------------------------------------------------------------!
subroutine delete_bound(bound)
implicit none
type(st_bound) :: bound


endsubroutine delete_bound





endmodule BOUND
