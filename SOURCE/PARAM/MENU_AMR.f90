!------------------------------------------------------------------------------!
! MODULE : MENU_AMR                       Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition des amr
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_AMR

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Definition des vecteurs 3D

implicit none

! -- Variables globales du module -------------------------------------------

! -- method of amr --
character, parameter :: amr_evolutive   = 'E'  ! full refinement each time
character, parameter :: amr_progressive = 'P'  ! progressive refinement (levels)

! -- periodicity --
character, parameter :: amr_cycle       = 'C'  ! only at each cycle
character, parameter :: amr_timestep    = 'T'  ! at each time step inside the cycle

! -- type of amr criteria --
character, parameter :: crit_sphere   = 'S'    ! geometrical criterion
character, parameter :: crit_boundary = 'B'    ! given number of cells from boundaries
character, parameter :: crit_facediff = 'F'    ! difference of quantities at faces

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_AMR : general numerical options 
!------------------------------------------------------------------------------!
type mnu_amr
  character             :: method      ! AMR method
  character             :: period      ! refinement step periodicity
  integer               :: maxlevel    ! max number of levels
  integer               :: degree      ! degree of cell refinement
  integer               :: nbcriter    ! number of criteria
  type(mnu_amrcrit), dimension(:), pointer &
                        :: criter      ! criteria array
endtype mnu_amr

!------------------------------------------------------------------------------!
! structure MNU_AMRCRIT : numerical options for refinement criteria
!------------------------------------------------------------------------------!
type mnu_amrcrit
  character             :: type        ! AMR criteria
  integer               :: level       ! level
  integer               :: quantity    ! quantite
  type(v3d)             :: center      ! center position (si necessaire)
  real(krp)             :: criterion   ! radius / threshold / etc
endtype mnu_amrcrit


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_amr
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine delete_mnu_amr(defamr)
implicit none
type(mnu_amr)  :: defamr

  if (defamr%nbcriter /= 0) deallocate(defamr%criter)

endsubroutine delete_mnu_amr


endmodule MENU_AMR
!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : MODULE creation
!------------------------------------------------------------------------------!




