!------------------------------------------------------------------------------!
! MODULE : MENU_ZONECOUPLING              Auteur : E. Radenac / J. Gressier
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Definition des methodes de couplage entre zones
!
!------------------------------------------------------------------------------!

module MENU_ZONECOUPLING

use TYPHMAKE   ! Definition de la precision et des types basiques
use DEFFIELD   ! Definition des champs de valeurs physiques pour les transferts
use ZONE_COUPLING

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure MNU_ZONECOUPLING : structures d'echanges entres zones
!------------------------------------------------------------------------------!
type mnu_zonecoupling
  character(len=shortname)   :: family     ! nom de famille de la CL
  character(len=shortname)   :: connzone   ! nom de la zone connectee par ce raccord
  character(len=shortname)   :: connfam    ! nom de famille de la CL connectee
                                           ! par ce raccord
  type(st_zonecoupling)      :: zcoupling  ! parametres de couplage
  real(krp)                  :: partcor    ! part de correction a faire par 
                                           ! iteration <=1
  integer                    :: typ_cor    ! type de repartition de correction
endtype mnu_zonecoupling 

! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_zonecoupling
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_ZONECOUPLING
!------------------------------------------------------------------------------!
subroutine delete_zonecoupling(zcoupl)
implicit none
type(mnu_zonecoupling)  :: zcoupl

call delete(zcoupl%zcoupling)

endsubroutine delete_zonecoupling

endmodule MENU_ZONECOUPLING

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): creation du module
!                      creation de delete
!------------------------------------------------------------------------------!


